package ideah.tree;

import com.google.common.collect.Iterables;
import com.intellij.formatting.Indent;
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

    public Iterable<? extends Located> getChildren() {
        return Iterables.concat(binds, sigs);
    }

    public void format() {
        for (Located child : getChildren()) {
            child.indent = Indent.getNormalIndent();
        }
    }
}
