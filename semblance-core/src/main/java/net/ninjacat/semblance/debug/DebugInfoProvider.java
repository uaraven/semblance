package net.ninjacat.semblance.debug;

/**
 * This interface should be implemented by all values that can store debugging information
 */
public interface DebugInfoProvider {

    /**
     * Returns source position information for a value.
     *
     * @return {@link SourceInfo}
     */
    SourceInfo getSourceInfo();
}
