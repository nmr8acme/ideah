package ideah.tree;

import com.google.common.collect.Iterables;
import ideah.tree.pat.Pat;

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
            int i = allChildren.indexOf(where);
            IndentBlock whereBlock = LocalBinds.createSubBlock(allChildren, i + 1, allChildren.size(), factory, where);
            allChildren.remove(where);
            allChildren.add(whereBlock);
        }
    }
}
