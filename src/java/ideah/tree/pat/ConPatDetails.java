package ideah.tree.pat;

import java.util.List;

public final class ConPatDetails {

    public final List<Pat> patterns; // todo: ???
    public final List<RecField> args;

    public ConPatDetails(List<Pat> patterns, List<RecField> args) {
        this.patterns = patterns;
        this.args = args;
    }
}
