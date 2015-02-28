package net.ninjacat.semblance.reader;

import net.ninjacat.semblance.data.LispCollection;
import net.ninjacat.semblance.data.SList;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.Vector;
import net.ninjacat.semblance.errors.UnexpectedEndRuntimeException;
import net.ninjacat.semblance.reader.macros.QuoteMacro;
import net.ninjacat.semblance.utils.Values;
import net.ninjacat.smooth.collections.Lists;
import org.hamcrest.CoreMatchers;
import org.junit.Before;
import org.junit.Test;

import java.util.Collections;
import java.util.List;

import static net.ninjacat.semblance.debug.SourceInfo.UNKNOWN;
import static net.ninjacat.semblance.reader.Token.integer;
import static net.ninjacat.semblance.reader.Token.symbol;
import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

public class ParserTest {

    private Parser parser;

    @Before
    public void setUp() throws Exception {
        parser = new Parser();
    }

    @Test
    public void shouldProduceSlist() throws Exception {

        LispCollection parse = parser.parse(Collections.<Token>emptyList());

        assertThat("Parser result should be SList", parse, CoreMatchers.instanceOf(SList.class));
    }

    @Test
    public void shouldParseSymbol() throws Exception {
        List<Token> tokens = Lists.of(symbol("symbol", UNKNOWN));

        LispCollection parse = parser.parse(tokens);

        assertThat("Should produce symbol", (SymbolAtom) parse.head(), is(Values.symbol("symbol")));
    }

    @Test
    public void shouldParseTwoSymbols() throws Exception {
        List<Token> tokens = Lists.of(symbol("one", UNKNOWN), symbol("another", UNKNOWN));

        LispCollection parse = parser.parse(tokens);

        assertThat("Should produce symbol 'one'", (SymbolAtom) parse.head(), is(Values.symbol("one")));
        assertThat("Should produce symbol 'another'", (SymbolAtom) parse.tail().head(), is(Values.symbol("another")));
    }

    @Test
    public void shouldParseVector() throws Exception {
        List<Token> tokens = Lists.of(Token.openBracket(UNKNOWN),
                symbol("one", UNKNOWN),
                integer("10", UNKNOWN), Token.closeBracket(UNKNOWN));

        LispCollection parse = parser.parse(tokens);

        assertThat("Should produce vector", (Vector) parse.head(),
                is(vector(Values.symbol("one"), number(10L))));
    }

    @Test(expected = UnexpectedEndRuntimeException.class)
    public void shouldFailToParseUnterminatedVector() throws Exception {
        List<Token> tokens = Lists.of(Token.openBracket(UNKNOWN),
                symbol("one", UNKNOWN),
                integer("10", UNKNOWN));

        parser.parse(tokens);
    }

    @Test
    public void shouldParseList() throws Exception {
        List<Token> tokens = Lists.of(Token.openParen(UNKNOWN),
                symbol("one", UNKNOWN),
                integer("10", UNKNOWN),
                Token.closeParen(UNKNOWN));

        LispCollection parse = parser.parse(tokens);

        assertThat("Should produce vector", (SList) parse.head(),
                is(list(Values.symbol("one"), number(10L))));
    }

    @Test(expected = UnexpectedEndRuntimeException.class)
    public void shouldFailToParseUnterminatedList() throws Exception {
        List<Token> tokens = Lists.of(Token.openParen(UNKNOWN),
                symbol("one", UNKNOWN),
                integer("10", UNKNOWN));

        parser.parse(tokens);
    }

    @Test()
    public void shouldReplaceQuoteMacroForList() throws Exception {
        List<Token> tokens = Lists.of(Token.special('\'', UNKNOWN),
                Token.openParen(UNKNOWN),
                Token.symbol("one", UNKNOWN),
                Token.closeParen(UNKNOWN));
        parser.registerReaderMacro(new QuoteMacro());

        LispCollection parse = parser.parse(tokens);

        assertThat("Should produce s-expression", parse.head(), instanceOf(SList.class));

        SList expr = (SList) parse.head();

        assertThat("Function call should be quote", (SymbolAtom) expr.head(), is(Values.symbol("quote")));
        assertThat("Function paramter should be list", (SList) expr.tail().head(), is(list(Values.symbol("one"))));
    }


    @Test()
    public void shouldReplaceQuoteMacroForSymbol() throws Exception {
        List<Token> tokens = Lists.of(Token.special('\'', UNKNOWN), symbol("one", UNKNOWN));
        parser.registerReaderMacro(new QuoteMacro());

        LispCollection parse = parser.parse(tokens);

        assertThat("Should produce s-expression", parse.head(), instanceOf(SList.class));

        SList expr = (SList) parse.head();

        assertThat("Function call should be quote", (SymbolAtom) expr.head(), is(Values.symbol("quote")));
        assertThat("Function paramter should be symbol", (SymbolAtom) expr.tail().head(), is(Values.symbol("one")));
    }

}
