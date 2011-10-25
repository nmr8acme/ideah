package ideah.tree.pat;

import ideah.tree.Ident;
import ideah.util.LineColRange;

public final class VarPat extends Pat {

    public final Ident name;

    public VarPat(LineColRange location, Ident name) {
        super(location);
        this.name = name;
    }
}
