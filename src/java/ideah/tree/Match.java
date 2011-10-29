package ideah.tree;

import com.google.common.collect.Iterables;
import ideah.tree.pat.Pat;

import java.util.Arrays;
import java.util.List;

public final class Match extends Located {

    public final List<Pat> params;
    // todo: Maybe LHsType - ???
    public final GRHSs rightHand;

    public Match(IRange location, List<Pat> params, GRHSs rightHand) {
        super(location);
        this.params = params;
        this.rightHand = rightHand;
    }

    protected Iterable<Located> getChildren() {
        return Iterables.concat(params, rightHand.getChildren());
    }

    @Override
    protected void rebuildStructure(RangeFactory factory) {
        Located where = findKeyword("where");
        if (where != null) {
            Iterable<? extends Located> whereDecls = rightHand.where.getChildren();
            WhereBinds whereBindsBlock = new WhereBinds(minMax(whereDecls, factory));
            for (Located whereDecl : whereDecls) {
                allChildren.remove(whereDecl);
                whereBindsBlock.allChildren.add(whereDecl);
            }
            Where whereBlock = new Where(minMax(Arrays.asList(whereBindsBlock, where), factory));
            whereBlock.allChildren.add(where);
            whereBlock.allChildren.add(whereBindsBlock);
            allChildren.remove(where);
            allChildren.add(whereBlock);
        }
    }

    @Override
    protected void format() {
        rightHand.format();
    }
}
