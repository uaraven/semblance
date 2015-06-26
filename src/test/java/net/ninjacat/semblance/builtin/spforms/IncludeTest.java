package net.ninjacat.semblance.builtin.spforms;

import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.errors.runtime.FileNotFoundException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.RootContext;
import net.ninjacat.smooth.collections.Lists;
import net.ninjacat.smooth.utils.Option;
import org.junit.Test;

import static net.ninjacat.semblance.utils.Values.*;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

public class IncludeTest {

    @Test(expected = FileNotFoundException.class)
    public void testShouldThrowExceptionIfFileNotFound() throws Exception {
        final Include include = new Include();
        final Context context = new RootContext();
        include.apply(context, list(string("non_existent")));
    }

    @Test
    public void testShouldLoadDefinitionsFromFile() throws Exception {
        final Include include = new Include();
        final Context context = new RootContext();
        context.setSourceFolders(Lists.of("jar:"));
        include.apply(context, list(string("test_program")));

        final Option<LispValue> addOp = context.findSymbol(symbol("add"));
        assertThat("Should bind add function in current context", addOp.isPresent(), is(true));
    }
}