use super::lexer::SourcePos;
use std::fmt;
use std::cell::RefCell;
use std::borrow::{Cow, IntoCow};

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
    pub fn new<T>(source: &'a str, filename: &'a str, pos: SourcePos, ty: Level, msg: T) -> Issue<'a>
            where T: IntoCow<'static, str> {
        Issue {
            source: source,
            filename: filename,
            pos: pos,
            msg: msg.into_cow(),
            ty: ty,
        }
    }
}

impl<'a> fmt::Display for Issue<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
    issues: RefCell<Vec<Issue<'a>>>,
    source: &'a str,
    filename: &'a str,
}

impl<'a> IssueTracker<'a> {
    pub fn new(filename: &'a str, source: &'a str) -> IssueTracker<'a> {
        IssueTracker {
            issues: RefCell::new(Vec::new()),
            source: source,
            filename: filename,
        }
    }

    pub fn new_issue<T>(&self, pos: SourcePos, ty: Level, msg: T) where T: IntoCow<'static, str> {
        let issue = Issue::new(self.source, self.filename, pos, ty, msg);
        self.issues.borrow_mut().push(issue);
    }

    pub fn is_ok(&self) -> bool {
        self.issues.borrow().iter().fold(true, |acc, ref item| acc & (item.ty != Level::Error))
    }
}

impl<'a> fmt::Display for IssueTracker<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for issue in self.issues.borrow().iter() {
            try!(write!(f, "{}\n", issue));
        }
        Ok(())
    }
}

