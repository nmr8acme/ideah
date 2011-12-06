package ideah.sdk;

import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.projectRoots.AdditionalDataConfigurable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.SdkModel;
import com.intellij.openapi.projectRoots.SdkModificator;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class HaskellSdkConfigurable implements AdditionalDataConfigurable {

    private final HaskellSdkConfigurableForm myForm;

    private Sdk mySdk;

    public HaskellSdkConfigurable(@NotNull SdkModel sdkModel, @NotNull SdkModificator sdkModificator) {
        myForm = new HaskellSdkConfigurableForm(sdkModel, sdkModificator);
    }

    public void setSdk(Sdk sdk) {
        mySdk = sdk;
    }

    public JComponent createComponent() {
        return myForm.getContentPanel();
    }

    public boolean isModified() {
        HaskellSdkAdditionalData data = (HaskellSdkAdditionalData) mySdk.getSdkAdditionalData();
        Sdk javaSdk = data != null ? data.getJavaSdk() : null;
        return javaSdk != myForm.getSelectedSdk();
    }

    public void apply() throws ConfigurationException {
        // todo
    }

    public void reset() {
        // todo
    }

    public void disposeUIResources() {
    }

    public void addJavaSdk(Sdk sdk) {
        myForm.addJavaSdk(sdk);
    }

    public void removeJavaSdk(Sdk sdk) {
        myForm.removeJavaSdk(sdk);
    }

    public void updateJavaSdkList(Sdk sdk, String previousName) {
        myForm.updateJdks(sdk, previousName);
    }

    public void internalJdkUpdate(Sdk sdk) {
        myForm.internalJdkUpdate(sdk);
    }
}
