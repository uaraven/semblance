package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.LispCollection;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.SList;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.errors.runtime.ParameterException;
import net.ninjacat.semblance.errors.runtime.ParameterValueExpectedException;
import net.ninjacat.semblance.errors.runtime.UnexpectedValueException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.smooth.utils.Option;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import static net.ninjacat.semblance.utils.Values.*;

/**
 * List of formal parameters.
 * <pre>
 * Supported parameter types:
 *   - positional parameters
 *   - optional parameters (param [default-value [supplied-flag]])
 *   - &amp;rest parameter
 * Named parameters are not yet supported.
 * </pre>
 * <p/>
 * Created on 28/02/15.
 */
public class Parameters implements Iterable<Parameter>, Serializable {
    private static final SymbolAtom OPTIONAL = symbol("&optional");
    private static final SymbolAtom REST = symbol("&rest");

    private final List<Parameter> formalParameters;
    private final Option<Parameter> restParameter;

    /**
     * Create new list of formal parameters from a definition list
     *
     * @param definitions List of parameters definitions.
     */
    public Parameters(final SList definitions) {
        formalParameters = new ArrayList<>();
        Sweeper sweeper = Sweeper.Normal;
        Parameter tempRest = null;
        for (final LispValue value : definitions) {
            if (OPTIONAL.equals(value)) {
                sweeper = Sweeper.Optional;
            } else if (REST.equals(value)) {
                sweeper = Sweeper.Rest;
            } else {
                final Parameter parameter = createParameter(value, sweeper);
                if (Sweeper.Rest == sweeper) {
                    tempRest = parameter;
                } else {
                    formalParameters.add(parameter);
                }
            }
        }
        if (null == tempRest && Sweeper.Rest == sweeper) {
            throw new ParameterException("&resp parameter not specified", definitions.getSourceInfo());
        }
        restParameter = Option.of(tempRest);
    }


    @Override
    public Iterator<Parameter> iterator() {
        return formalParameters.iterator();
    }

    /**
     * Gets list of formal parameters.
     * @return List of {@link Parameter}
     */
    public List<Parameter> getFormalParameters() {
        return formalParameters;
    }

    /**
     * Gets &amp;rest parameter, if defined.
     * @return Optional value of &amp;rest parameter;
     */
    public Option<Parameter> getRestParameter() {
        return restParameter;
    }

    /**
     * Assigns actual parameters according to formal parameter definition. Assignment is done in supplied context.
     *
     * @param context          Context in which assign parameters.
     * @param actualParameters List of actual parameters.
     */
    public void apply(final Context context, final LispCollection actualParameters) {
        final LispCollection evaluated = context.evaluateList(actualParameters);
        applyList(context, evaluated);
    }

    /**
     * Assigns actual parameters according to formal parameter definition. Assignment is done in supplied context.
     * Does not evaluates actual parameters, expressions are assigned as is.
     *
     * @param context          Context in which assign parameters.
     * @param actualParameters List of actual parameters.
     */
    public void bindExpressions(final Context context, final LispCollection actualParameters) {
        applyList(context, actualParameters);
    }

    private void applyList(final Context context, final LispCollection evaluated) {
        SList params = toSList(evaluated);
        // assign all available parameters
        for (final Parameter parameter : this) {
            if (params.isNil()) {
                if (parameter.isRequired()) {
                    throw new ParameterValueExpectedException(parameter.getName(), evaluated.getSourceInfo());
                } else {
                    parameter.bindInContext(context, null);
                }
            } else {
                final LispValue param = params.head();
                params = asSList(params.tail());
                parameter.bindInContext(context, param);
            }
        }
        if (restParameter.isPresent()) {
            restParameter.get().bindInContext(context, params);
        }
    }

    private Parameter createParameter(final LispValue value, final Sweeper sweeper) {
        switch (sweeper) {
            case Normal:
                return createNormalParameter(value);
            case Optional:
                return createOptionalParamter(value);
            case Rest:
                return createRestParameter(value);
        }
        throw new UnexpectedValueException(value);
    }

    private Parameter createRestParameter(final LispValue value) {
        return new RestParameter(asSymbol(value));
    }

    private Parameter createNormalParameter(final LispValue value) {
        return new PositionalParameter(asSymbol(value));
    }

    private Parameter createOptionalParamter(final LispValue value) {
        if (isSymbol(value)) {
            return new OptionalParameter(asSymbol(value),
                    Option.<LispValue>absent(), Option.<SymbolAtom>absent());
        } else if (isList(value)) {
            final SList list = asSList(value);
            final SymbolAtom name = asSymbol(list.head());
            final LispValue defaultValue = list.tail().head();
            final Option<SymbolAtom> flagName = 2 < list.length()
                    ? Option.of(asSymbol(list.tail().tail().head()))
                    : Option.<SymbolAtom>absent();
            return new OptionalParameter(name, Option.of(defaultValue), flagName);
        } else {
            throw new UnexpectedValueException(value);
        }
    }

    private enum Sweeper {
        Normal,
        Optional,
        Rest
    }
}
