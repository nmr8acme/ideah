package ideah.tree.pat;

import ideah.tree.Ident;

public final class VarPat extends Pat {

    public final Ident name;

    public VarPat(Ident name) {
        this.name = name;
    }
}
