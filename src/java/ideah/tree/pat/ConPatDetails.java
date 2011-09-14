package ideah.tree.pat;

import java.util.List;

public final class ConPatDetails {

    public final Pat pattern; // todo: ???
    public final List<RecField> args;

    public ConPatDetails(Pat pattern, List<RecField> args) {
        this.pattern = pattern;
        this.args = args;
    }
}
