#![feature(macro_metavar_expr)]
#![feature(type_alias_impl_trait)]
#![feature(anonymous_lifetime_in_impl_trait)]
//#![no_std]
extern crate alloc;

use core::ops::Range;
use core::{iter::Peekable, ops::RangeFrom};

use alloc::borrow::ToOwned;
use alloc::{boxed::Box, string::String, vec, vec::Vec};

type Loc = core::ops::Range<usize>;

#[derive(Debug, Default)]
struct ErrRange(Option<usize>, Option<usize>);

pub enum ErrorType<'a> {
    Message(String),
    Escape(AstNode<'a>),
}

pub struct Error<'a> {
    message: ErrorType<'a>,
    range: ErrRange,
}

impl<'a> Error<'a> {
    fn new(message: String, range: impl Into<ErrRange>) -> Self {
        Error {
            message: ErrorType::Message(message),
            range: range.into(),
        }
    }

    pub fn display(&self, input: &str) -> String {
        let msg = match &self.message {
            ErrorType::Message(x) => x,
            _ => panic!(),
        };
        let mut st = String::new();
        st.push_str(input);
        st.push('\n');
        let start = if self.range.0.is_none() {
            input.len()
        } else {
            self.range.0.unwrap()
        };
        for _ in 0..start {
            st.push(' ');
        }
        st.push('^');
        let rlen = if self.range.1.is_none() {
            0
        } else if self.range.1.unwrap() < self.range.0.unwrap() {
            input.len() - start
        } else {
            self.range.1.unwrap() - start
        };
        for _ in 2..rlen {
            st.push('-')
        }
        if rlen > 0 {
            st.push('^');
        }
        st.push('\n');
        for _ in 0..start {
            st.push(' ');
        }
        st + &msg + " :3"
    }
}

impl From<RangeFrom<usize>> for ErrRange {
    fn from(value: RangeFrom<usize>) -> Self {
        ErrRange(Some(value.start), None)
    }
}

impl From<Range<usize>> for ErrRange {
    fn from(value: Range<usize>) -> Self {
        ErrRange(Some(value.start), Some(value.end))
    }
}

impl<'a> From<&'static str> for Error<'a> {
    fn from(v: &'static str) -> Self {
        Error {
            message: ErrorType::Message(v.to_owned()),
            range: ErrRange(None, None),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum RedirType {
    Stdout,
    Stderr,
}

#[derive(Debug, PartialEq, Eq)]
pub enum OpenOrClose {
    OpenExpand,
    Close,
}

#[derive(Debug)]
pub enum CommandBit {
    Str(String, Loc),
    Quotes(Vec<CommandBit>, Loc),
    Pipe(Loc),
    Redir(RedirType, Loc),
    Dollar(Loc),
    Control(OpenOrClose, Loc),
}

impl CommandBit {
    const fn is_op(&self) -> bool {
        !matches!(
            self,
            Self::Str(..) | Self::Dollar(..) | Self::Control(..) | Self::Quotes(..)
        )
    }

    const fn precedence(&self) -> OperatorPrecedence {
        match self {
            Self::Pipe(..) => OperatorPrecedence::Pipe,
            Self::Str(..) => OperatorPrecedence::Command,
            Self::Redir(..) => OperatorPrecedence::Redirect,
            Self::Dollar(..) | Self::Control(..) | Self::Quotes(..) => unreachable!(),
        }
    }

    const fn expects_atom_after(&self) -> bool {
        assert!(self.is_op());
        match self {
            Self::Pipe(..) => false,
            Self::Redir(..) => true,
            _ => unreachable!(),
        }
    }

    const fn max_atom_length(&self) -> Option<u32> {
        assert!(self.is_op());
        match self {
            Self::Pipe(..) => None,
            Self::Redir(..) => Some(1),
            _ => unreachable!(),
        }
    }
}

impl From<(String, Loc)> for CommandBit {
    fn from(value: (String, Loc)) -> Self {
        match value.0.as_str() {
            "|" => CommandBit::Pipe(value.1),
            ">" => CommandBit::Redir(RedirType::Stdout, value.1),
            "2>" => CommandBit::Redir(RedirType::Stderr, value.1),
            _ => CommandBit::Str(value.0, value.1),
        }
    }
}

pub fn split(t: &str) -> Result<Vec<CommandBit>, Error> {
    let mut chars = t.chars().peekable();
    let mut cbs = vec![];
    let mut innercbs = vec![];
    let mut stri = String::new();
    let mut startloc = 0;
    let mut endloc = 0;
    let mut quote_stack: Option<char> = None;
    macro_rules! build_str {
        () => {
            #[allow(unused_assignments)]
            {
                if (startloc != endloc) {
                    let ostri = stri;
                    stri = String::new();
                    if quote_stack.is_none() {
                        if !ostri.trim().is_empty() {
                            cbs.push(CommandBit::Str(ostri, startloc..endloc));
                        }
                    } else {
                        innercbs.push(CommandBit::Str(ostri, startloc..endloc));
                    }
                    startloc = endloc;
                }
            }
        };
    }
    macro_rules! tokenizer {
        (
            match($quote_stack:ident) $ch:expr => {
                    $($x:pat $( $(if $ifs:expr)?, $($spec:ident)?)? => $z:expr),+
            }
        ) => {
            match $ch {
                $(
                    $x
                    $(
                        $( if $ifs )?
                        $(
                            if $quote_stack.is_none() ${ignore(spec)}
                        )?
                    )?
                    =>
                    $z
                ),+
            }
        };
    }
    while let Some(char) = chars.next() {
        tokenizer!(match(quote_stack) char => {
            '$' => {
                if chars.next_if(|x| *x == '(').is_some() {
                    build_str!();
                    endloc += 1;
                    cbs.push(CommandBit::Control(
                        OpenOrClose::OpenExpand,
                        startloc..endloc,
                    ))
                } else {
                    build_str!();
                    cbs.push(CommandBit::Dollar(startloc..endloc))
                }
            },
            '|', spec => {
                build_str!();
                cbs.push(CommandBit::Pipe(startloc..endloc));
            },
            ')', => {
                build_str!();
                cbs.push(CommandBit::Control(OpenOrClose::Close, startloc..endloc))
            },
            '\\' => {
                let next = chars.next().ok_or(Error::new("Expected char".into(), endloc..))?;
                if next != '\n' {
                    stri.push(next)
                }
                endloc += 1;
            },
            '2' if chars.next_if(|x| *x == '>').is_some() && quote_stack.is_none(), => {
                build_str!();
                endloc += 1;
                cbs.push(CommandBit::Redir(RedirType::Stderr, startloc..endloc))
            },
            '>', spec => {
                build_str!();
                cbs.push(CommandBit::Redir(RedirType::Stdout, startloc..endloc))
            },
            x @ ('"' | '\'') => {
                if quote_stack.is_some() {
                    if *quote_stack.as_ref().unwrap() != x {
                        stri.push(x);
                    } else {
                        build_str!();
                        quote_stack = None;
                      cbs.append(&mut innercbs);
                      innercbs.clear();
                    }
                } else {
                    build_str!();
                    quote_stack = Some(x);
                }
            },
            x if x.is_whitespace()&& quote_stack.is_none(), => {
                build_str!();
            },
            x => stri.push(x)
        });
        endloc += 1;
    }
    build_str!();
    if !innercbs.is_empty() {
        panic!()
    }
    Ok(cbs)
}

#[derive(Debug)]
pub enum AstNode<'a> {
    Cmd(&'a str, Loc),
    Variable(&'a str, Loc),
    Expansion(Box<AstNode<'a>>, Loc),
    JustCmd(Vec<AstNode<'a>>, Loc),
    Pipe(Box<AstNode<'a>>, Loc, Box<AstNode<'a>>, Loc),
    Redirect(Box<AstNode<'a>>, RedirType, Loc, Box<AstNode<'a>>, Loc),
}

impl AstNode<'_> {
    fn get_loc(&self) -> Loc {
        match self {
            AstNode::JustCmd(_, loc) => loc.clone(),
            AstNode::Pipe(_, _, _, loc2) => loc2.clone(),
            AstNode::Redirect(_, _, _, _, loc2) => loc2.clone(),
            AstNode::Cmd(_, loc) => loc.clone(),
            AstNode::Variable(_, loc) => loc.clone(),
            AstNode::Expansion(_, loc) => loc.clone(),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum OperatorPrecedence {
    Pipe,
    Redirect,
    Command,
}

impl OperatorPrecedence {
    fn next(&self) -> Self {
        match self {
            Self::Pipe => Self::Redirect,
            Self::Redirect => Self::Command,
            Self::Command => unreachable!(),
        }
    }
}

macro_rules! wrap_outer_error {
    ($e:expr) => {{
        let _res = $e;
        if let Ok(x) = _res {
            x
        } else {
            return Err(core::mem::transmute(_res.unwrap_err()));
        }
    }};
}

pub fn parse<'b>(
    c: &mut Peekable<impl Iterator<Item = &'b CommandBit>>,
) -> Result<AstNode<'b>, Error<'static>> {
    unsafe {
        let p1: Result<AstNode<'b>, Error<'b>> = parse_1(
            wrap_outer_error!(parse_atom(c, 0, Ctx::None)),
            OperatorPrecedence::Redirect,
            c,
            Ctx::None,
        );
        Ok(wrap_outer_error!(p1))
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Ctx {
    None,
    Substitution,
}

fn parse_1<'a, T>(
    mut lhs: AstNode<'a>,
    prec: OperatorPrecedence,
    stream: &mut Peekable<T>,
    ctx: Ctx,
) -> Result<AstNode<'a>, Error<'a>>
where
    T: Iterator<Item = &'a CommandBit>,
{
    while stream.peek().map_or(false, |x| x.is_op()) {
        let op = stream.next().unwrap(); // advance next token

        let mut rhs = parse_atom(stream, op.max_atom_length().map_or(0, |x| x), ctx)?;
        while stream.peek().map_or(false, |x| {
            x.is_op() && x.precedence() > op.precedence() && !op.expects_atom_after()
        }) {
            rhs = parse_1(rhs, prec.next(), stream, ctx)?;
        }
        let fullrange = lhs.get_loc().start..rhs.get_loc().end;
        lhs = match op {
            CommandBit::Pipe(l) => {
                AstNode::Pipe(Box::new(lhs), l.clone(), Box::new(rhs), fullrange)
            }
            CommandBit::Redir(t, l) => {
                AstNode::Redirect(Box::new(lhs), *t, l.clone(), Box::new(rhs), fullrange)
            }
            _ => unreachable!(),
        }
    }
    Ok(lhs)
}

fn parse_atom<'a>(
    stream: &mut Peekable<impl Iterator<Item = &'a CommandBit>>,
    max: u32,
    ctx: Ctx,
) -> Result<AstNode<'a>, Error<'a>> {
    let mut children: Vec<_> = vec![];
    while let Some(tok) = stream.next_if(|x| !x.is_op()) {
        if max != 0 && children.len() >= max as usize {
            break;
        }

        match dbg!(tok) {
            CommandBit::Str(s, loc) => children.push(AstNode::Cmd(s, loc.clone())),
            CommandBit::Dollar(loc) => {
                let t = stream
                    .next_if(|x| matches!(x, CommandBit::Str(..)))
                    .ok_or(Error::new("Need an ident".into(), loc.end..))?;
                let (s, l) = match t {
                    CommandBit::Str(s, l) => (s, l),
                    _ => unreachable!(),
                };

                children.push(AstNode::Variable(s, loc.start..l.end))
            }
            CommandBit::Control(oe, loc) => match oe {
                OpenOrClose::Close => {
                    if ctx == Ctx::None {
                        return Err(Error::new("Unexpected close".into(), loc.clone()));
                    } else if ctx == Ctx::Substitution {
                      if children.len() == 0 {
                        let range = loc.clone();
                        return Err(Error::new("Expected a command".into(), range))
                      }
                        let range = children.first().unwrap().get_loc().start
                            ..children.last().unwrap().get_loc().end;
                        return Err(Error {
                            message: ErrorType::Escape(AstNode::JustCmd(children, range)),
                            range: (0..0).into(),
                        });
                    }
                }
                OpenOrClose::OpenExpand => {
                    let mut lhs = parse_atom(stream, 0, Ctx::Substitution);
                  dbg!(stream.peek());
                    if let Ok(x) = lhs {
                        lhs = parse_1(x, OperatorPrecedence::Redirect, stream, Ctx::Substitution);
                    }

                    match lhs {
                        Ok(_) => panic!(),
                        Err(x) => {
                            if matches!(x.message, ErrorType::Message(..)) {
                                return Err(x);
                            }
                            match x.message {
                                ErrorType::Escape(x) => {
                                    let range = loc.start..x.get_loc().end;
                                    children.push(AstNode::Expansion(Box::new(x), range))
                                }
                                _ => panic!(),
                            }
                        }
                    }
                }
            },
            _ => panic!(),
        }
    }
    if children.is_empty() {
        return Err("Expected a command".into());
    }
    if ctx==Ctx::Substitution {
      return Err("Unmatched substitution brackets".into())
    }
    let range = children.first().unwrap().get_loc().start..children.last().unwrap().get_loc().end;

    Ok(AstNode::JustCmd(children, range))
}
