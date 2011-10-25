package ideah.tree;

import ideah.tree.pat.Pat;
import ideah.util.LineColRange;

import java.util.List;

public final class Match extends Located {

    public final List<Pat> params;
    // todo: Maybe LHsType - ???
    public final GRHSs rightHand;

    public Match(LineColRange location, List<Pat> params, GRHSs rightHand) {
        super(location);
        this.params = params;
        this.rightHand = rightHand;
    }
}
