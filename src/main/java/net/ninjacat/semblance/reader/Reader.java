package net.ninjacat.semblance.reader;

import net.ninjacat.semblance.data.SList;
import net.ninjacat.semblance.errors.compile.ParsingException;
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

    public SList read(InputStream stream) throws ParsingException {
        ReaderStream readerStream = ReaderStream.readStream(stream);
        Parser parser = new Parser();

        registerMacros(readerStream, parser);

        return parser.parse(readerStream.tokenize());
    }

    public SList readString(String text) throws ParsingException {
        ReaderStream readerStream = ReaderStream.readString(text);
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
