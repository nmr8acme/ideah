package ideah.tree.pat;

import ideah.tree.Ident;

public final class ConPat extends Pat {

    public final Ident constructor; // todo: ???
    public final ConPatDetails details;

    public ConPat(Ident constructor, ConPatDetails details) {
        this.constructor = constructor;
        this.details = details;
    }
}
