package net.ninjacat.semblance.reader;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.NilCollection;
import net.ninjacat.semblance.data.SList;
import net.ninjacat.semblance.errors.compile.ParsingException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.DefaultContext;
import net.ninjacat.semblance.evaluator.RootContext;
import net.ninjacat.semblance.reader.macros.QuoteMacro;

import java.io.InputStream;
import java.util.HashSet;
import java.util.Set;

/**
 * Created on 25/02/15.
 */
public class Reader {

    private final Set<ReaderMacro> macros;

    public Reader() {
        macros = new HashSet<>();
        macros.add(new QuoteMacro());
    }

    public LispValue run(InputStream stream) throws ParsingException {
        SList program = read(stream);
        return doRun(program);
    }

    public LispValue run(String text) throws ParsingException {
        SList program = readString(text);
        return doRun(program);
    }

    public SList read(InputStream stream) throws ParsingException {
        ReaderStream readerStream = ReaderStream.readStream(stream);
        return parse(readerStream);
    }

    public SList readString(String text) throws ParsingException {
        ReaderStream readerStream = ReaderStream.readString(text);
        return parse(readerStream);
    }

    private LispValue doRun(SList program) {
        Context executionContext = DefaultContext.namelessChildContext(new RootContext());
        LispValue evaluated = NilCollection.INSTANCE;
        for (LispValue sExpression : program) {
            evaluated = executionContext.evaluate(sExpression);
        }
        return evaluated;
    }

    private SList parse(ReaderStream readerStream) throws ParsingException {
        Parser parser = new Parser();

        registerMacros(readerStream, parser);

        return parser.parse(readerStream.tokenize());
    }

    private void registerMacros(ReaderStream readerStream, Parser parser) {
        for (ReaderMacro macro : macros) {
            readerStream.registerSpecial(macro.getMacroCharacter().charAt(0));
            parser.registerReaderMacro(macro);
        }
    }
}
