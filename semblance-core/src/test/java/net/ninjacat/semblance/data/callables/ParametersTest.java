package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.NilCollection;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.errors.runtime.ParameterException;
import net.ninjacat.semblance.errors.runtime.TypeMismatchException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.utils.Values;
import org.junit.Test;

import java.util.List;
import java.util.Optional;

import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.*;

@SuppressWarnings({"NonBooleanMethodNameMayNotStartWithQuestion", "DuplicateStringLiteralInspection", "InstanceMethodNamingConvention"})
public class ParametersTest {

    @Test
    public void shouldParseStandardParameters() throws Exception {
        final Parameters parameters = new Parameters(list(symbol("param1"), symbol("param2")));

        final List<Parameter> params = parameters.getFormalParameters();

        assertThat("Should parse standard params",
                (PositionalParameter) params.get(0), is(new PositionalParameter(symbol("param1"))));
        assertThat("Should parse standard params",
                (PositionalParameter) params.get(1), is(new PositionalParameter(symbol("param2"))));
    }

    @Test
    public void shouldParseOptionalParamNoDefaults() throws Exception {
        final Parameters parameters = new Parameters(list(symbol("param1"), symbol("&optional"),
                symbol("opt1")));

        final List<Parameter> params = parameters.getFormalParameters();

        assertThat("Should parse standard params",
                (PositionalParameter) params.get(0), is(new PositionalParameter(symbol("param1"))));
        assertThat("Should parse optional params",
                (OptionalParameter) params.get(1), is(new OptionalParameter(
                        symbol("opt1"), Optional.empty(), Optional.empty())));
    }


    @Test
    public void shouldParseOptionalParamWithDefault() throws Exception {
        final Parameters parameters = new Parameters(list(symbol("param1"), symbol("&optional"),
                list(symbol("opt1"), number(10L))
        ));

        final List<Parameter> params = parameters.getFormalParameters();

        assertThat("Should parse standard params",
                (PositionalParameter) params.get(0), is(new PositionalParameter(symbol("param1"))));
        assertThat("Should parse optional params",
                (OptionalParameter) params.get(1), is(new OptionalParameter(
                        symbol("opt1"), Optional.of(number(10L)), Optional.empty())));
    }

    @Test
    public void shouldParseOptionalParamWithDefaultAndFlag() throws Exception {
        final Parameters parameters = new Parameters(list(symbol("param1"), symbol("&optional"),
                list(symbol("opt1"), number(10L), symbol("opt1-supplied-p"))
        ));

        final List<Parameter> params = parameters.getFormalParameters();

        assertThat("Should parse standard params",
                (PositionalParameter) params.get(0), is(new PositionalParameter(symbol("param1"))));
        assertThat("Should parse optional params",
                (OptionalParameter) params.get(1), is(new OptionalParameter(
                        symbol("opt1"), Optional.of(number(10L)), Optional.of(symbol("opt1-supplied-p")))));
    }

    @Test
    public void shouldParseRestParameter() throws Exception {
        final Parameters parameters = new Parameters(list(symbol("&rest"), symbol("rest1")));

        parameters.getFormalParameters();

        assertThat("Should parse &rest params",
                parameters.getRestParameter().isPresent(), is(true));
    }

    @Test
    public void shouldParseStandardAndRestParameter() throws Exception {
        final Parameters parameters = new Parameters(list(symbol("param1"), symbol("&rest"), symbol("rest1")));

        final List<Parameter> params = parameters.getFormalParameters();

        assertThat("Should parse standard params",
                (PositionalParameter) params.get(0), is(new PositionalParameter(symbol("param1"))));
        assertThat("Should parse &rest params",
                parameters.getRestParameter().isPresent(), is(true));
    }

    @Test
    public void shouldParseStandardOptionalAndRestParameter() throws Exception {
        final Parameters parameters = new Parameters(list(symbol("param1"), symbol("&optional"),
                list(symbol("opt1"), number(10L), symbol("opt1-supplied-p")),
                symbol("&rest"), symbol("rest1")));

        final List<Parameter> params = parameters.getFormalParameters();

        assertThat("Should parse standard params",
                (PositionalParameter) params.get(0), is(new PositionalParameter(symbol("param1"))));
        assertThat("Should parse optional params",
                (OptionalParameter) params.get(1), is(new OptionalParameter(
                        symbol("opt1"), Optional.of(number(10L)), Optional.of(symbol("opt1-supplied-p")))));
        assertThat("Should parse &rest params",
                parameters.getRestParameter().isPresent(), is(true));
    }

    @Test(expected = TypeMismatchException.class)
    public void shouldFailWhenParamListContainsString() throws Exception {
        final Parameters parameters = new Parameters(list(string("param1")));

        parameters.getFormalParameters();
    }

    @Test(expected = TypeMismatchException.class)
    public void shouldFailWhenParamListContainsList() throws Exception {
        final Parameters parameters = new Parameters(list(list(string("param1"))));

        parameters.getFormalParameters();
    }

    @Test
    public void shouldBindActualParameter() throws Exception {
        final SymbolAtom param = symbol("param1");
        final Parameters parameters = new Parameters(list(param));
        final Context context = mock(Context.class);
        when(context.evaluateList(list(number(10)))).thenReturn(list(number(10)));

        parameters.apply(context, list(number(10)));

        verify(context).bind(param, number(10));
    }

    @Test
    public void shouldBindMultipleActualParameters() throws Exception {
        final SymbolAtom param1 = symbol("param1");
        final SymbolAtom param2 = symbol("param2");
        final Parameters parameters = new Parameters(list(param1, param2));
        final Context context = mock(Context.class);
        final SList actualParams = list(number(10), string("v"));
        when(context.evaluateList(actualParams)).thenReturn(actualParams);

        parameters.apply(context, actualParams);

        verify(context).bind(param1, number(10));
        verify(context).bind(param2, string("v"));
    }

    @Test
    public void shouldBindOptionalParameter() throws Exception {
        final SymbolAtom param1 = symbol("param1");
        final SymbolAtom param2 = symbol("param2");
        final Parameters parameters = new Parameters(list(param1, symbol("&optional"), param2));
        final Context context = mock(Context.class);
        final SList actualParams = list(number(10), string("v"));
        when(context.evaluateList(actualParams)).thenReturn(actualParams);

        parameters.apply(context, actualParams);

        verify(context).bind(param1, number(10));
        verify(context).bind(param2, string("v"));
    }

    @Test
    public void shouldBindNilAsOmittedOptionalParameter() throws Exception {
        final SymbolAtom param1 = symbol("param1");
        final SymbolAtom param2 = symbol("param2");
        final Parameters parameters = new Parameters(list(param1, symbol("&optional"), param2));
        final Context context = mock(Context.class);
        final SList actualParams = list(number(10));
        when(context.evaluateList(actualParams)).thenReturn(actualParams);

        parameters.apply(context, actualParams);

        verify(context).bind(param1, number(10));
        verify(context).bind(param2, NilCollection.INSTANCE);
    }


    @Test
    public void shouldBindNilAsOmittedOptionalParameterWithDefault() throws Exception {
        final SymbolAtom param1 = symbol("param1");
        final SymbolAtom param2 = symbol("param2");
        final Parameters parameters = new Parameters(list(param1, symbol("&optional"), list(param2, string("v"))));
        final Context context = mock(Context.class);
        final SList actualParams = list(number(10));
        when(context.evaluateList(actualParams)).thenReturn(actualParams);
        when(context.evaluate(string("v"))).thenReturn(string("v"));

        parameters.apply(context, actualParams);

        verify(context).bind(param1, number(10));
        verify(context).bind(param2, string("v"));
    }

    @Test
    public void shouldBindNilAsOmittedOptionalParameterWithDefaultAndFlag() throws Exception {
        final SymbolAtom param1 = symbol("param1");
        final SymbolAtom param2 = symbol("param2");
        final Parameters parameters = new Parameters(list(param1, symbol("&optional"), list(param2, string("v"), symbol("flag"))));
        final Context context = mock(Context.class);
        final SList actualParams = list(number(10));
        when(context.evaluateList(actualParams)).thenReturn(actualParams);
        when(context.evaluate(string("v"))).thenReturn(string("v"));

        parameters.apply(context, actualParams);

        verify(context).bind(param1, number(10));
        verify(context).bind(param2, string("v"));
        verify(context).bind(symbol("flag"), Values.T);
    }


    @Test(expected = ParameterException.class)
    public void shouldFailWhenRestParameterIsNotSpecified() throws Exception {
        final SymbolAtom param = symbol("&rest");
        final Parameters parameters = new Parameters(list(param));
        final Context context = mock(Context.class);
        final SList actualParameters = list(number(10), number(12));
        when(context.evaluateList(actualParameters)).thenReturn(actualParameters);

        parameters.apply(context, actualParameters);
    }


    @Test
    public void shouldBindRestParameter() throws Exception {
        final SymbolAtom rest = symbol("&rest");
        final SymbolAtom restParam = symbol("rest_param");
        final Parameters parameters = new Parameters(list(rest, restParam));
        final Context context = mock(Context.class);
        final SList actualParameters = list(number(10), number(12));
        when(context.evaluateList(actualParameters)).thenReturn(actualParameters);

        parameters.apply(context, actualParameters);

        verify(context).bind(restParam, actualParameters);
    }



    @Test
    public void shouldBindEmptyRestParameter() throws Exception {
        final SymbolAtom param = symbol("param");
        final SymbolAtom rest = symbol("&rest");
        final SymbolAtom restParam = symbol("rest_param");
        final Parameters parameters = new Parameters(list(param, rest, restParam));
        final Context context = mock(Context.class);
        final SList actualParameters = list(number(10));
        when(context.evaluateList(actualParameters)).thenReturn(actualParameters);

        parameters.apply(context, actualParameters);

        verify(context).bind(param, number(10));
        verify(context).bind(restParam, NilCollection.INSTANCE);
    }


}
