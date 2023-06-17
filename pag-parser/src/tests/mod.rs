// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use ariadne::Source;
use strip_ansi_escapes::Writer;
mod failure;

fn write_error<S: AsRef<str>, N: AsRef<str>>(input: S, name: N) -> String {
    let mut buffer = Vec::<u8>::new();
    {
        let result = crate::generate_parser(input.as_ref()).unwrap_err();
        let reports = result.to_reports(name.as_ref());
        let mut cache = (name.as_ref(), Source::from(input.as_ref()));
        let mut writer = Writer::new(&mut buffer);
        for i in reports {
            i.write(&mut cache, &mut writer).unwrap();
        }
    }
    String::from_utf8(buffer).unwrap()
}
