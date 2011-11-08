package ideah.tree;

import com.google.common.collect.Iterables;
import ideah.tree.decl.Bind;
import ideah.tree.decl.SigDecl;

import java.util.Arrays;
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

    public static IndentBlock createSubBlock(List<Located> allChildren, int from, int to, RangeFactory factory, Located where) {
        Iterable<Located> whereDecls = allChildren.subList(from, to);
        IndentBlock whereBindsBlock = new IndentBlock(Located.minMax(whereDecls, factory));
        int n = to - from;
        for (int i = 0; i < n; i++) {
            Located local = allChildren.remove(from);
            whereBindsBlock.allChildren.add(local);
        }
        IndentBlock whereBlock = new IndentBlock(Located.minMax(Arrays.asList(where, whereBindsBlock), factory));
        whereBlock.allChildren.add(where);
        whereBlock.allChildren.add(whereBindsBlock);
        return whereBlock;
    }
}
