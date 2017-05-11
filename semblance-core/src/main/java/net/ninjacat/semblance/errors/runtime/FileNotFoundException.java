package net.ninjacat.semblance.errors.runtime;

import net.ninjacat.semblance.debug.SourceInfo;

import java.util.List;

import static java.util.stream.Collectors.joining;

/**
 * Thrown when included semblance file cannot be found on class path.
 */
public class FileNotFoundException extends SemblanceRuntimeException {
    private static final long serialVersionUID = -8000652454660254185L;
    private static final String NOT_FOUND = "File '%s' not found in path [%s]";

    /**
     * Creates a new instance of FileNotFoundException
     *
     * @param fileName   Name of the file which was not found
     * @param paths      List of paths where file was searched for
     * @param sourceInfo Source information for {@code include} statement
     */
    public FileNotFoundException(final String fileName,
                                 final List<String> paths,
                                 final SourceInfo sourceInfo) {
        super(String.format(NOT_FOUND, fileName, paths.stream().collect(joining(","))), sourceInfo);
    }
}
