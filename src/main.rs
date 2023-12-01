#![feature(try_blocks)]
use autoshell::{parse, Error};

fn main() {
    let st = "ls -la | grep txt > \"$(echo OUTFILE).txt\" | echo $?";
    let opt: Result<(), Error> = try {
        let spl = dbg!(autoshell::split(st)?);

        let mut iter = spl.iter().peekable();

        let parsed = parse(&mut iter)?;
        dbg!(parsed);
    };
    if let Err(e) = opt {
        println!("{}", e.display(st));
    }
}
