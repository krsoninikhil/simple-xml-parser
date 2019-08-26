pub fn match_literal<'a>(expected: &'static str) -> impl Parser<'a, ()> {
    move |input: &'a str| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

pub fn identifier(input: &str) -> ParseResult<String> {
    let mut matched = String::new();
    let mut chars = input.chars();

    match chars.next() {
        Some(next) if next.is_alphabetic() => matched.push(next),
        _ => return Err(input),
    }

    while let Some(next) = chars.next() {
        if next.is_alphanumeric() || next == '-' {
            matched.push(next)
        } else {
            break
        }
    }

    return Ok((&input[matched.len()..], matched))
}

type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

pub trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;
}

impl<'a, F, Output> Parser<'a, Output> for F
    where F: Fn(&'a str) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
    where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| parser1.parse(input).and_then(|(next_input, result1)| {
        parser2.parse(next_input)
            .map(|(remaining_input, result2)| (remaining_input, (result1, result2)))
    })
}

fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
    where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input| parser.parse(input)
        .map(|(next_input, result)| (next_input, map_fn(result)))
}

pub fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
    where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(left, _right)| left)
}

pub fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
    where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_left, right)| right)
}

pub fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
    where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        // first match
        if let Ok((next, first_value)) = parser.parse(input) {
            input = next;
            result.push(first_value);
        } else {
            return Err(input);
        }
        // repeated match
        while let Ok((next_input, next_value)) = parser.parse(input) {
            input = next_input;
            result.push(next_value);
        }

        Ok((input, result))
    }
}

pub fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
    where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();
        while let Ok((next_input, next_value)) = parser.parse(input) {
            input = next_input;
            result.push(next_value);
        }

        Ok((input, result))
    }
}

pub fn any_char(input: &str) -> ParseResult<char> {
    match input.chars().next() {
        Some(first_char) => Ok((&input[first_char.len_utf8()..], first_char)),
        _ => Err(input),
    }
}

pub fn pred<'a, P, F, R>(parser: P, predicate: F) -> impl Parser<'a, R>
    where
    P: Parser<'a, R>,
    F: Fn(&R) -> bool,
{
    move |input| {
        if let Ok((next, value)) = parser.parse(input) {
            if predicate(&value) {
                return Ok((next, value));
            }
        }
        Err(input)
    }
}

pub fn whitespace_char<'a>() -> impl Parser<'a, char> {
    pred(any_char, |c| c.is_whitespace())
}

fn space1<'a>() impl Parser<'a, Vec<char>> {
    one_or_more(whitespace_char())
}

fn space0<'a>() impl Parser<'a, Vec<char>> {
    zero_or_more(whitespace_char())
}

#[test]
fn literal_parser() {
    let parse_div = match_literal("<div>");

    assert_eq!(
        Ok(("", ())),
        parse_div.parse("<div>")
    );
    assert_eq!(
        Ok(("</div>", ())),
        parse_div.parse("<div></div>")
    );
    assert_eq!(
        Err("<div id=\"test\"></div>"),
        parse_div.parse("<div id=\"test\"></div>")
    );
}

#[test]
fn identifier_parser() {
    assert_eq!(
        Ok(("", "an-identifier".to_string())),
        identifier("an-identifier")
    );
    assert_eq!(
        Ok((" and more", "an-identifier".to_string())),
        identifier("an-identifier and more")
    );
    assert_eq!(
        Err("!not-an-identifier"),
        identifier("!not-an-identifier")
    );
}

#[test]
fn pair_combinator() {
    let parse_open_tag = pair(match_literal("<"), identifier);

    assert_eq!(
        Ok(("/>", ((), "hr".to_string()))),
        parse_open_tag.parse("<hr/>")
    );
    assert_eq!(Err("oops"), parse_open_tag.parse("oops"));
    assert_eq!(Err("!oops"), parse_open_tag.parse("!oops"));
}

#[test]
fn right_combinator() {
    let parse_open_tag = right(match_literal("<"), identifier);

    assert_eq!(
        Ok(("/>", "hr".to_string())),
        parse_open_tag.parse("<hr/>")
    );
}

#[test]
fn one_or_more_combinator() {
    let parser = one_or_more(match_literal("ha"));
    assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("hahaha"));
    assert_eq!(Err("ahah"), parser.parse("ahah"));
    assert_eq!(Err(""), parser.parse(""));
}

#[test]
fn zero_or_more_combinator() {
    let parser = zero_or_more(match_literal("ha"));
    assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("hahaha"));
    assert_eq!(Ok(("ahah", vec![])), parser.parse("ahah"));
    assert_eq!(Ok(("", vec![])), parser.parse(""));
}

#[test]
fn predicate_combinator() {
    let parser = pred(any_char, |c| *c == 'o');
    assert_eq!(Ok(("mg", 'o')), parser.parse("omg"));
    assert_eq!(Err("lol"), parser.parse("lol"));
}
