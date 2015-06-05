use super::tokens::SourcePos;
use super::common::Context;

use std::fmt;
use std::borrow::Cow;

#[derive(Debug, PartialEq)]
pub enum Level {
    Error,
    Warning,
}

#[derive(Debug)]
pub struct Issue<'a> {
    pub source: &'a str,
    pub filename: &'a str,
    pub pos: SourcePos,
    pub msg: Cow<'static, str>,
    pub ty: Level,
}

impl<'a> Issue<'a> {
    pub fn new(source: &'a str,
               filename: &'a str,
               pos: SourcePos,
               ty: Level,
               msg: Cow<'static, str>) -> Issue<'a> {
        Issue {
            source: source,
            filename: filename,
            pos: pos,
            msg: msg,
            ty: ty,
        }
    }
}

impl<'a> fmt::Display for Issue<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // oh god why
        let line = &self.source[self.pos.line_index..];
        let line = line[..line.find('\n').unwrap_or(line.len())].to_string();
        let line = line.replace("\t", " ");
        let align = self.filename.len() + self.pos.to_string().len() + 3;
        write!(f, "{0}+{1} ┬ {2:?}: {3}\n{4:>5$} {6}\n{7:>5$}{8:─>9$}┘",
               self.filename, self.pos, self.ty, self.msg,
               "┃", align, line,
               "└", "", self.pos.index - self.pos.line_index + 1
        )
    }
}

#[derive(Debug)]
pub struct IssueTracker<'a> {
    issues: Vec<Issue<'a>>,
}

impl<'a> IssueTracker<'a> {
    pub fn new() -> IssueTracker<'a> {
        IssueTracker {
            issues: Vec::new(),
        }
    }

    pub fn new_issue<T>(&mut self, ctxt: &'a Context, pos: SourcePos, ty: Level, msg: T)
            where T: Into<Cow<'static, str>> {
        let issue = Issue::new(&ctxt.source, &ctxt.filename, pos, ty, msg.into());
        self.issues.push(issue);
    }

    pub fn is_ok(&self) -> bool {
        self.issues.iter().fold(true, |acc, ref item| acc & (item.ty != Level::Error))
    }
}

impl<'a> fmt::Display for IssueTracker<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for issue in self.issues.iter() {
            try!(write!(f, "{}\n", issue));
        }
        Ok(())
    }
}

