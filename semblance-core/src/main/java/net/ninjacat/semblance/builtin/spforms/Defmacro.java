package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.callables.Macro;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.evaluator.Context;

import static net.ninjacat.semblance.utils.Values.asCollection;
import static net.ninjacat.semblance.utils.Values.asSymbol;

/**
 * <pre>
 *  (defmacro name (parameters) &rest body)
 * </pre>
 */
public class Defmacro extends SpecialForm {

    private static final long serialVersionUID = -3163706610883209916L;

    /**
     * Creates a new instance of defmacro special form
     */
    public Defmacro() {
        super("defmacro", "name", "params", "&body");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        final SymbolAtom name = asSymbol(parameters.head());
        final LispCollection params = asCollection(parameters.tail().head());
        final LispCollection body = asCollection(parameters.tail().tail());

        final Macro macro = new Macro(name, params, body);

        context.bind(name, macro);
        return macro;
    }
}
