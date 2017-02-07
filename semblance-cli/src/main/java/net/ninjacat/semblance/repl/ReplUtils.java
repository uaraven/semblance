package net.ninjacat.semblance.repl;

/**
 * Utility methods for repl
 */
public final class ReplUtils {
    private ReplUtils() {
    }

    /**
     * Counts parenthesis in the string. Returns false if parenthesis do not match. This means
     * user input is not finished yet and is continued on next line
     *
     * @param ln Input string
     * @return true or false
     */
    public static boolean isMatchingParentheses(final CharSequence ln) {
        int openParens = 0;
        int closeParens = 0;
        int openBrackets = 0;
        int closeBrackets = 0;
        for (int i = 0; i < ln.length(); i++) {
            final char c = ln.charAt(i);
            switch (c) {
                case '(':
                    openParens += 1;
                    break;
                case ')':
                    closeParens += 1;
                    break;
                case '[':
                    openBrackets += 1;
                    break;
                case ']':
                    closeBrackets += 1;
                    break;
            }
        }
        return openParens == closeParens && openBrackets == closeBrackets;
    }
}
