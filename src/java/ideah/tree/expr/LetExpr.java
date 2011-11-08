package ideah.tree.expr;

import com.google.common.collect.Iterables;
import ideah.tree.*;

import java.util.Arrays;

public final class LetExpr extends Expression {

    public final LocalBinds localBinds;
    public final Expression expression;

    public LetExpr(IRange location, LocalBinds localBinds, Expression expression) {
        super(location);
        this.localBinds = localBinds;
        this.expression = expression;
    }

    protected Iterable<Located> getChildren() {
        return Iterables.concat(localBinds.getChildren(), Arrays.asList(expression));
    }

    @Override
    protected void rebuildStructure(RangeFactory factory) {
        Located let = findKeyword("let");
        Located in = findKeyword("in");
        if (let != null && in != null) {
            int from = allChildren.indexOf(let);
            int to = allChildren.indexOf(in);
            IndentBlock letBinds = LocalBinds.createSubBlock(allChildren, from + 1, to, factory, let);
            allChildren.remove(from);
            allChildren.add(from, letBinds);
            IndentBlock inExpr = LocalBinds.createSubBlock(allChildren, from + 2, allChildren.size(), factory, in);
            allChildren.remove(from + 1);
            allChildren.add(from + 1, inExpr);
            // todo: indent binds relative to let?
            // todo: align let/in?
        }
    }
}
