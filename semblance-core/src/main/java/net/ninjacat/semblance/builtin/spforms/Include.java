package net.ninjacat.semblance.builtin.spforms;

import com.google.common.base.Joiner;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.data.StringAtom;
import net.ninjacat.semblance.data.callables.SpecialForm;
import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.SList;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.compile.ParsingException;
import net.ninjacat.semblance.errors.compile.ParsingIOException;
import net.ninjacat.semblance.errors.compile.ParsingRuntimeException;
import net.ninjacat.semblance.errors.runtime.FileNotFoundException;
import net.ninjacat.semblance.evaluator.Context;

import java.io.*;
import java.util.List;
import java.util.Optional;

import static net.ninjacat.semblance.evaluator.SourceUtils.readCompiled;
import static net.ninjacat.semblance.evaluator.SourceUtils.readProgram;
import static net.ninjacat.semblance.utils.Values.asString;

/**
 * Includes a file
 */
public class Include extends SpecialForm {

    private static final long serialVersionUID = -5338822452321779274L;
    private static final Joiner PATH_JOINER = Joiner.on(File.separator);

    /**
     * Creates a new instance of Include class
     */
    public Include() {
        super("include", "filename");
    }

    @Override
    public LispValue apply(final Context context, final LispCollection parameters) {
        final StringAtom sourceName = asString(context.evaluateList(parameters).head());
        try {
            final SList source = getCompiled(context, sourceName, parameters.getSourceInfo());
            return context.evaluateBlock(source);
        } catch (final ParsingException e) {
            throw new ParsingRuntimeException("Failed to include " + sourceName, e, SourceInfo.UNKNOWN);
        }
    }

    private static Optional<SList> loadFile(final String sourceFile) throws ParsingException {
        if (sourceFile.endsWith(".sc")) {
            return loadCompiled(sourceFile);
        } else if (sourceFile.endsWith(".smbl")) {
            return loadAndCompile(sourceFile);
        } else {
            return Optional.empty();
        }
    }

    private static Optional<SList> findInPath(final List<String> paths, final String sourceFile, final String... extensions)
            throws ParsingException {
        for (final String path : paths) {
            for (final String ext : extensions) {
                if (path.startsWith("jar:")) {
                    final Optional<SList> loaded = loadFromResource(PATH_JOINER.join(path, sourceFile + "." + ext).substring(4));
                    if (loaded.isPresent()) {
                        return loaded;
                    }
                } else {
                    final File file = new File(PATH_JOINER.join(path, sourceFile + "." + ext));
                    if (file.exists()) {
                        return loadFile(file.getAbsolutePath());
                    }
                }
            }
        }
        return Optional.empty();
    }

    private static Optional<SList> loadFromResource(final String resourceName) throws ParsingException {
        try (final InputStream asStream = Include.class.getResourceAsStream(resourceName)) {
            if (asStream == null) {
                return Optional.empty();
            } else {
                if (resourceName.endsWith(".smbl")) {
                    return Optional.ofNullable(readProgram(asStream));
                } else if (resourceName.endsWith(".sc")) {
                    return Optional.ofNullable(readCompiled(asStream));
                } else {
                    return Optional.empty();
                }
            }
        } catch (final IOException e) {
            throw new ParsingIOException("Cannot read " + resourceName, e);
        }
    }

    private static Optional<SList> loadAndCompile(final String sourceFile) throws ParsingException {
        final File source = new File(sourceFile);
        if (source.exists()) {
            try (final InputStream is = new BufferedInputStream(new FileInputStream(sourceFile))) {
                return Optional.ofNullable(readProgram(is));
            } catch (final IOException e) {
                throw new ParsingIOException("Cannot load " + sourceFile, e);
            }
        } else {
            return Optional.empty();
        }
    }

    private static Optional<SList> loadCompiled(final String sourceFile) throws ParsingException {
        final File source = new File(sourceFile);
        if (source.exists()) {
            try (final InputStream is = new BufferedInputStream(new FileInputStream(sourceFile))) {
                return Optional.ofNullable(readCompiled(is));
            } catch (final IOException e) {
                throw new ParsingIOException("Cannot load " + sourceFile, e);
            }
        } else {
            return Optional.empty();
        }
    }

    private static SList getCompiled(final Context context, final StringAtom sourceName, final SourceInfo sourceInfo)
            throws ParsingException {
        final String sourceFile = sourceName.getValue();
        final Optional<SList> srcSList = loadFile(sourceFile);
        if (srcSList.isPresent()) {
            return srcSList.get();
        } else {
            final Optional<SList> program = findInPath(context.getSourceFolders(), sourceFile, "sc", "smbl");
            return program.orElseThrow(
                    () -> new FileNotFoundException(sourceFile, context.getSourceFolders(), sourceInfo));
        }
    }

}