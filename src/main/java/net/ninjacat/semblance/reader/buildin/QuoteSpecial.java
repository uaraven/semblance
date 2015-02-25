package net.ninjacat.semblance.reader.buildin;

import net.ninjacat.semblance.errors.TermExpectedException;
import net.ninjacat.semblance.reader.ReaderSpecial;
import net.ninjacat.semblance.reader.ReaderStream;
import net.ninjacat.semblance.reader.Token;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

/**
 * Created on 25/02/15.
 */
public class QuoteSpecial implements ReaderSpecial {
    @Override
    public char getSpecialTrigger() {
        return '\'';
    }

    @Override
    public Collection<Token> replaceTokenStream(String trigger, ReaderStream reader) throws TermExpectedException {
        Token token = reader.nextToken();
        if (token.getType() != Token.TokenType.OpenParens) {
            throw new TermExpectedException("(", token);
        }
        List<Token> tokens = new ArrayList<>();
        tokens.add(new Token("(", Token.TokenType.OpenParens, token.getSourceInfo()));
        tokens.add(new Token("quote", Token.TokenType.Symbol, token.getSourceInfo()));
        return Collections.unmodifiableCollection(tokens);
    }
}
