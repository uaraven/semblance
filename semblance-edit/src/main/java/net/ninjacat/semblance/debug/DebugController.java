package net.ninjacat.semblance.debug;

import net.ninjacat.semblance.Interpreter;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.errors.compile.ParsingException;

@SuppressWarnings("OptionalUsedAsFieldOrParameterType")
public class DebugController {

    private final Interpreter interpreter;
    private DebuggerUi debuggerUi;

    public DebugController(final Interpreter interpreter) {
        this.interpreter = interpreter;
    }

    public void setDebuggerUi(final DebuggerUi debuggerUi) {

        this.debuggerUi = debuggerUi;
    }

    public void run() {
        if (debuggerUi != null) {
            final String sourceCode = debuggerUi.getSourceCode();
            try {
                final LispValue result = interpreter.run(sourceCode);
                debuggerUi.setResult(result);
            } catch (final ParsingException e) {
                debuggerUi.setError(e.getMessage(), e.getSourceInfo());
            }
        }
    }
}
