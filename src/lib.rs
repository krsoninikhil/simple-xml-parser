pub fn match_literal(expected: &'static str) -> impl Fn(&str) -> Result<(&str, ()), &str> {
    move |input| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

pub fn identifier(input: &str) -> Result<(&str, String), &str> {
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

pub fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
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

#[test]
fn literal_parser() {
    let parse_div = match_literal("<div>");

    assert_eq!(
        Ok(("", ())),
        parse_div("<div>")
    );
    assert_eq!(
        Ok(("</div>", ())),
        parse_div("<div></div>")
    );
    assert_eq!(
        Err("<div id=\"test\"></div>"),
        parse_div("<div id=\"test\"></div>")
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
        parse_open_tag("<hr/>")
    );
    assert_eq!(Err("oops"), parse_open_tag("oops"));
    assert_eq!(Err("!oops"), parse_open_tag("!oops"));
}
