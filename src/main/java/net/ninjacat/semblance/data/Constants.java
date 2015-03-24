package net.ninjacat.semblance.data;

/**
 * Constants
 */
public final class Constants {

    /**
     * Symbol constants
     */
    public static final SymbolAtom NONE = new SymbolAtom("");
    public static final SymbolAtom TRUE = new SymbolAtom("T");
    public static final SymbolAtom FALSE = new SymbolAtom("F");

    private Constants() {
    }

    /**
     * Name of internal comma function
     */
    public static final class HiddenFunctions {
        /**
         * Symbol for comma special form.
         */
        public static final SymbolAtom COMMA = new SymbolAtom("#--eval-me--#");
        /**
         * Symbol for unwrapping list form.
         */
        public static final SymbolAtom UNWRAP = new SymbolAtom("#--unwrap-me--#");

        private HiddenFunctions() {
        }
    }
}
