package net.ninjacat.semblance.evaluator;

import net.ninjacat.semblance.builtin.spforms.*;
import net.ninjacat.semblance.builtin.spforms.arithmetic.*;
import net.ninjacat.semblance.builtin.spforms.comparison.*;
import net.ninjacat.semblance.builtin.spforms.logic.And;
import net.ninjacat.semblance.builtin.spforms.logic.Not;
import net.ninjacat.semblance.builtin.spforms.logic.Or;
import net.ninjacat.semblance.data.NilCollection;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.callables.SpecialForm;

import static net.ninjacat.semblance.utils.Values.symbol;

/**
 * Base context for every program.
 * <p/>
 * Created on 01/03/15.
 */
public class RootContext extends BaseContext {
    /**
     * Creates new instance of root context
     */
    public RootContext() {
        super("/", null);

        bind(symbol("nil"), NilCollection.INSTANCE);
        bind(symbol("T"), SymbolAtom.TRUE);
        bind(symbol("F"), SymbolAtom.FALSE);

        bindSpecialForms();
    }

    @Override
    protected Context createChild(final String name) {
        return LocalContext.namedChildContext(name, this);
    }

    private void bindSpecialForms() {
        bindForm(new Var());

        bindForm(new Quote());
        bindForm(new PrintLn());

        bindForm(new Fn());
        bindForm(new Defmacro());

        bindForm(new Add());
        bindForm(new Sub());
        bindForm(new Div());
        bindForm(new Mul());
        bindForm(new Mod());

        bindForm(new Progn());
        bindForm(new Let());

        bindForm(new If());
        bindForm(new Equal());
        bindForm(new NotEqual());
        bindForm(new GreaterThan());
        bindForm(new GreaterEqual());
        bindForm(new LessThan());
        bindForm(new LessEqual());
        bindForm(new And());
        bindForm(new Not());
        bindForm(new Or());
    }

    private void bindForm(final SpecialForm form) {
        bind(form.name(), form);
    }
}
