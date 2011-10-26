package ideah.tree;

import ideah.tree.decl.Bind;
import ideah.tree.decl.SigDecl;

import java.util.List;

public final class LocalBinds {

    public final List<Bind> binds;
    public final List<SigDecl> sigs;


    public LocalBinds(List<Bind> binds, List<SigDecl> sigs) {
        this.binds = binds;
        this.sigs = sigs;
    }
}
