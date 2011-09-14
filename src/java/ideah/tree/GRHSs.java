package ideah.tree;

import java.util.List;

public final class GRHSs {

    public final List<GRHS> grhss;
    public final LocalBinds where;

    public GRHSs(List<GRHS> grhss, LocalBinds where) {
        this.grhss = grhss;
        this.where = where;
    }
}
