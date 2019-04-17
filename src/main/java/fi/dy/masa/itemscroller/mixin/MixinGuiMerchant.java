package fi.dy.masa.itemscroller.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.itemscroller.config.Configs;
import fi.dy.masa.itemscroller.event.InputHandler;
import fi.dy.masa.itemscroller.event.RenderEventHandler;
import fi.dy.masa.itemscroller.villager.VillagerData;
import fi.dy.masa.itemscroller.villager.VillagerDataStorage;
import net.minecraft.client.gui.GuiMerchant;

@Mixin(GuiMerchant.class)
public abstract class MixinGuiMerchant
{
    @Inject(method = "initGui", at = @At("RETURN"))
    private void setRestorePreviousPage(CallbackInfo ci)
    {
        if (Configs.Toggles.VILLAGER_TRADE_LIST.getBooleanValue())
        {
            GuiMerchant gui = (GuiMerchant) (Object) this;

            if (Configs.Generic.VILLAGER_TRADE_LIST_REMEMBER_PAGE.getBooleanValue())
            {
                VillagerData data = VillagerDataStorage.getInstance().getDataForLastInteractionTarget();

                if (data != null)
                {
                    InputHandler.changeTradePage(gui, data.getLastPage());
                }
            }

            RenderEventHandler.instance().createVillagerTradeListWidget(gui);
        }
    }
}
