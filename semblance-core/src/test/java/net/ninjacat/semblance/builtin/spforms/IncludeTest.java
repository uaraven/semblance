package net.ninjacat.semblance.builtin.spforms;

import com.google.common.collect.ImmutableList;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.errors.runtime.FileNotFoundException;
import net.ninjacat.semblance.evaluator.Context;
import net.ninjacat.semblance.evaluator.RootContext;
import org.junit.Test;

import java.util.Optional;

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
        context.setSourceFolders(ImmutableList.of("jar:"));
        include.apply(context, list(string("test_program")));

        final Optional<LispValue> addOp = context.findSymbol(symbol("add"));
        assertThat("Should bind add function in current context", addOp.isPresent(), is(true));
    }
}