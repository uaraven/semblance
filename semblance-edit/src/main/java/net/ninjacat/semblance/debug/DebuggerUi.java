package net.ninjacat.semblance.debug;

import net.ninjacat.semblance.data.LispValue;

/**
 * TODO: Write JavaDoc
 */
public interface DebuggerUi {

    String getSourceCode();

    void setError(String message, SourceInfo sourceInfo);

    void setResult(LispValue result);
}
