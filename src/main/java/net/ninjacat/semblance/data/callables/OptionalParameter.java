package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.NilCollection;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.utils.Values;
import net.ninjacat.smooth.utils.Option;

/**
 * Created on 28/02/15.
 */
public class OptionalParameter extends BaseParameter {
    private final Option<LispValue> defaultValue;
    private final Option<SymbolAtom> suppliedFlagName;

    public OptionalParameter(SymbolAtom name,
                             Option<LispValue> defaultValue,
                             Option<SymbolAtom> suppliedFlagName) {
        super(name);
        this.defaultValue = defaultValue;
        this.suppliedFlagName = suppliedFlagName;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        OptionalParameter that = (OptionalParameter) o;

        if (!defaultValue.equals(that.defaultValue)) return false;
        if (!suppliedFlagName.equals(that.suppliedFlagName)) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = defaultValue.hashCode();
        result = 31 * result + suppliedFlagName.hashCode();
        return result;
    }

    @Override
    public boolean isRequired() {
        return false;
    }

    @Override
    public void setInContext(Context context, LispValue actualValue) {
        if (actualValue == null) {
            if (defaultValue.isPresent()) {
                context.bind(getName(), context.evaluate(defaultValue.get()));
                bindSupplied(context, true);
            } else {
                context.bind(getName(), NilCollection.INSTANCE);
                bindSupplied(context, false);
            }
        } else {
            context.bind(getName(), actualValue);
        }
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder("&optional ");
        builder.append(getName());
        if (defaultValue.isPresent()) {
            builder.append(" ").append(defaultValue.get());
        }
        if (suppliedFlagName.isPresent()) {
            builder.append(" ").append(suppliedFlagName.get());
        }
        return builder.toString();
    }

    private void bindSupplied(Context context, boolean isSupplied) {
        if (suppliedFlagName.isPresent()) {
            context.bind(suppliedFlagName.get(), isSupplied ? Values.T : Values.F);
        }
    }
}
