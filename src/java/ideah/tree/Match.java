package ideah.tree;

import ideah.tree.pat.Pat;

import java.util.List;

public final class Match extends Located {

    public final List<Pat> params;
    // todo: Maybe LHsType - ???
    public final GRHSs rightHand;

    public Match(List<Pat> params, GRHSs rightHand) {
        this.params = params;
        this.rightHand = rightHand;
    }
}
