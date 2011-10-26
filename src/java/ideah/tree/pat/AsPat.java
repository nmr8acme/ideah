package ideah.tree.pat;

import ideah.tree.Ident;
import ideah.util.LineColRange;

public final class AsPat extends Pat {

    public final Ident name;
    public final Pat pattern;

    public AsPat(LineColRange location, Ident name, Pat pattern) {
        super(location);
        this.name = name;
        this.pattern = pattern;
    }
}
