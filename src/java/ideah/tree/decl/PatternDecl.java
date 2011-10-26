package ideah.tree.decl;

import ideah.tree.GRHSs;
import ideah.tree.pat.Pat;
import ideah.util.LineColRange;

public final class PatternDecl extends Bind {

    public final Pat pattern;
    public final GRHSs rightHand;

    public PatternDecl(LineColRange location, Pat pattern, GRHSs rightHand) {
        super(location);
        this.pattern = pattern;
        this.rightHand = rightHand;
    }
}
