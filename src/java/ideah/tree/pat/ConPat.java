package ideah.tree.pat;

import ideah.tree.Ident;
import ideah.util.LineColRange;

public final class ConPat extends Pat {

    public final Ident constructor; // todo: ???
    public final ConPatDetails details;

    public ConPat(LineColRange location, Ident constructor, ConPatDetails details) {
        super(location);
        this.constructor = constructor;
        this.details = details;
    }
}
