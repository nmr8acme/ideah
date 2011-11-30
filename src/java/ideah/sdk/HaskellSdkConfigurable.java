package ideah.sdk;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.projectRoots.*;
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

    //  @Override
    public boolean isModified() {
        HaskellSdkAdditionalData data = (HaskellSdkAdditionalData) mySdk.getSdkAdditionalData();
        Sdk javaSdk = data != null ? data.getJavaSdk() : null;
        return javaSdk != myForm.getSelectedSdk();
    }

    //  @Override
    public void apply() throws ConfigurationException {
        HaskellSdkAdditionalData newData = new HaskellSdkAdditionalData(mySdk, myForm.getSelectedSdk());
//    newData.setBuildTarget(myForm.getSelectedBuildTarget());
        final SdkModificator modificator = mySdk.getSdkModificator();
        modificator.setSdkAdditionalData(newData);
        ApplicationManager.getApplication().runWriteAction(new Runnable() {
            public void run() {
                modificator.commitChanges();
            }
        });
    }

    //  @Override
    public void reset() {
        if (mySdk == null) {
            return;
        }
        SdkAdditionalData data = mySdk.getSdkAdditionalData();
        if (!(data instanceof HaskellSdkAdditionalData)) {
            return;
        }
        HaskellSdkAdditionalData androidData = (HaskellSdkAdditionalData) data;
//    AndroidPlatform platform = androidData.getAndroidPlatform();
//    myForm.init(androidData.getJavaSdk(), mySdk, platform != null ? androidData.getBuildTarget(platform.getSdk()) : null);
        myForm.init(androidData.getJavaSdk(), mySdk, null);
    }

    //  @Override
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
