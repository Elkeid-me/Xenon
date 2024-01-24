use std::fs;

enum State {
    Code,
    CodeWithBackSlash,
    CodeWithSlash,
    StringLiteral,
    StringLiteralWithEscape,
    Comment,
    CxxComment,
    CxxCommentWithBackSlash,
    CommentWithStar,
}

fn code_fun(c: char) -> (State, Option<char>, Option<char>) {
    match c {
        '/' => (State::CodeWithSlash, None, None),
        '\\' => (State::CodeWithBackSlash, None, None),
        '"' => (State::StringLiteral, Some(c), None),
        _ => (State::Code, Some(c), None),
    }
}
fn code_with_back_slash_fun(c: char) -> (State, Option<char>, Option<char>) {
    match c {
        '\\' => (State::CodeWithBackSlash, Some('\\'), None),
        '\n' => (State::Code, None, None),
        _ => (State::Code, Some('\\'), Some(c)),
    }
}
fn code_with_slash_fun(c: char) -> (State, Option<char>, Option<char>) {
    match c {
        '/' => (State::CxxComment, None, None),
        '*' => (State::Comment, None, None),
        _ => (State::Code, Some('/'), Some(c)),
    }
}
fn string_literal_fun(c: char) -> (State, Option<char>, Option<char>) {
    match c {
        '\\' => (State::StringLiteralWithEscape, None, None),
        '"' => (State::Code, Some(c), None),
        _ => (State::StringLiteral, Some(c), None),
    }
}
fn string_literal_with_escape_fun(c: char) -> (State, Option<char>, Option<char>) {
    match c {
        '\n' => (State::StringLiteral, None, None),
        _ => (State::StringLiteral, Some('\\'), Some(c)),
    }
}
fn comment_fun(c: char) -> (State, Option<char>, Option<char>) {
    match c {
        '*' => (State::CommentWithStar, None, None),
        _ => (State::Comment, None, None),
    }
}
fn cxx_comment_fun(c: char) -> (State, Option<char>, Option<char>) {
    match c {
        '\n' => (State::Code, Some(c), None),
        '\\' => (State::CxxCommentWithBackSlash, None, None),
        _ => (State::CxxComment, None, None),
    }
}
fn cxx_comment_with_back_slash_fun(c: char) -> (State, Option<char>, Option<char>) {
    match c {
        '\\' => (State::CxxCommentWithBackSlash, None, None),
        _ => (State::CxxComment, None, None),
    }
}
fn comment_with_star_fun(c: char) -> (State, Option<char>, Option<char>) {
    match c {
        '/' => (State::Code, Some(' '), None),
        '*' => (State::CommentWithStar, None, None),
        _ => (State::Comment, None, None),
    }
}

pub fn preprocess(code: &String) -> String {
    let mut new_code = String::new();
    let mut state = State::Code;
    let mut new_char_1: Option<char>;
    let mut new_char_2: Option<char>;
    for c in code.chars() {
        (state, new_char_1, new_char_2) = match state {
            State::Code => code_fun(c),
            State::CodeWithSlash => code_with_slash_fun(c),
            State::CodeWithBackSlash => code_with_back_slash_fun(c),
            State::Comment => comment_fun(c),
            State::CxxComment => cxx_comment_fun(c),
            State::CommentWithStar => comment_with_star_fun(c),
            State::StringLiteral => string_literal_fun(c),
            State::StringLiteralWithEscape => string_literal_with_escape_fun(c),
            State::CxxCommentWithBackSlash => cxx_comment_with_back_slash_fun(c),
        };
        if let Some(new_char) = new_char_1 {
            new_code.push(new_char);
        }
        if let Some(new_char) = new_char_2 {
            new_code.push(new_char);
        }
    }
    new_code
}
