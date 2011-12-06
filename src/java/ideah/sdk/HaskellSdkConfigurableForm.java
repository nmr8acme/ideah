package ideah.sdk;

import com.intellij.openapi.fileChooser.FileChooserDescriptor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.SdkModel;
import com.intellij.openapi.projectRoots.SdkModificator;
import com.intellij.openapi.roots.OrderRootType;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class HaskellSdkConfigurableForm {

    private JPanel myContentPanel;
    private JTextField myGhcOptionsTextField;
    private TextFieldWithBrowseButton myGhcLibPathTFWBB;
    private TextFieldWithBrowseButton myCabalPathTFWBB;
    private JLabel ghcLibPathLabel;
    private JLabel cabalPathLabel;
    private JLabel ghcOptionsLabel;

    private final SdkModel mySdkModel;

    public HaskellSdkConfigurableForm(@NotNull SdkModel sdkModel, @NotNull final SdkModificator sdkModificator) {
        mySdkModel = sdkModel;
        myGhcLibPathTFWBB.addBrowseFolderListener("Path to GHC lib directory", "Path to GHC lib Directory",
            null, new FileChooserDescriptor(false, true, false, false, false, false)); //todo: set project instead of null
        myCabalPathTFWBB.addBrowseFolderListener("Path to cabal installation directory", "Path to Cabal Installation Directory",
            null, new FileChooserDescriptor(false, true, false, false, false, false)); //todo: set project instead of null
    }

    @NotNull
    public JPanel getContentPanel() {
        return myContentPanel;
    }

    @Nullable
    public Sdk getSelectedSdk() {
        return null; // todo
    }

    @Nullable
    public void init(Sdk jdk, Sdk androidSdk, Object buildTarget) {
        // todo
    }

    private void updateJdks() {
        // todo
    }

    public void addJavaSdk(Sdk sdk) {
        // todo
    }

    public void removeJavaSdk(Sdk sdk) {
//        // todo
    }

    public void updateJdks(Sdk sdk, String previousName) {
        // todo
    }

    public void internalJdkUpdate(final Sdk sdk) {
        // todo
    }
}
