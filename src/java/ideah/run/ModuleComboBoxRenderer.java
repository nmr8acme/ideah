package ideah.run;

import com.intellij.openapi.module.Module;

import javax.swing.*;
import java.awt.*;

final class ModuleComboBoxRenderer extends JLabel implements ListCellRenderer {

    public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
        if (value instanceof Module) {
            setText(((Module) value).getName());
        }
        return this;
    }
}
