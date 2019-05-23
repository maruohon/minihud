package fi.dy.masa.itemscroller.mixin;

import javax.annotation.Nullable;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.itemscroller.config.Configs;
import fi.dy.masa.itemscroller.event.InputHandler;
import fi.dy.masa.itemscroller.gui.widgets.WidgetTradeList;
import fi.dy.masa.itemscroller.util.IGuiMerchant;
import fi.dy.masa.itemscroller.villager.VillagerData;
import fi.dy.masa.itemscroller.villager.VillagerDataStorage;
import net.minecraft.client.gui.GuiMerchant;
import net.minecraft.client.gui.inventory.GuiContainer;
import net.minecraft.entity.IMerchant;
import net.minecraft.inventory.Container;
import net.minecraft.village.MerchantRecipeList;

@Mixin(GuiMerchant.class)
public abstract class MixinGuiMerchant extends GuiContainer implements IGuiMerchant
{
    @Shadow
    @Final
    private IMerchant merchant;

    @Shadow
    private int selectedMerchantRecipe;

    @Nullable
    private WidgetTradeList widgetTradeList;

    public MixinGuiMerchant(Container inventorySlotsIn)
    {
        super(inventorySlotsIn);
    }

    @Nullable
    @Override
    public WidgetTradeList getTradeListWidget()
    {
        return this.widgetTradeList;
    }

    @Override
    public int getSelectedMerchantRecipe()
    {
        return this.selectedMerchantRecipe;
    }

    @Override
    public void setSelectedMerchantRecipe(int index)
    {
        this.selectedMerchantRecipe = index;
    }

    @Inject(method = "initGui", at = @At("RETURN"))
    private void initTradeListWidget(CallbackInfo ci)
    {
        if (Configs.Toggles.VILLAGER_TRADE_LIST.getBooleanValue())
        {
            VillagerData data = VillagerDataStorage.getInstance().getDataForLastInteractionTarget();

            if (data != null)
            {
                GuiMerchant gui = (GuiMerchant) (Object) this;

                if (Configs.Generic.VILLAGER_TRADE_LIST_REMEMBER_PAGE.getBooleanValue())
                {
                    InputHandler.changeTradePage(gui, data.getLastPage());
                }

                int x = this.guiLeft - 106 + 4;
                int y = this.guiTop;

                this.widgetTradeList = new WidgetTradeList(x, y, gui, data);
            }
        }
    }

    @Inject(method = "drawScreen",
            at = @At(value = "FIELD",
                     target = "Lnet/minecraft/client/gui/GuiMerchant;selectedMerchantRecipe:I"), cancellable = true)
    private void recipeIndexCheck(int mouseX, int mouseY, float partialTicks, CallbackInfo ci)
    {
        MerchantRecipeList trades = this.merchant.getRecipes(this.mc.player);

        if (trades != null && this.selectedMerchantRecipe >= trades.size())
        {
            InputHandler.changeTradePage((GuiMerchant) (Object) this, 0);
            ci.cancel();
        }
    }

    @Inject(method = "drawScreen", at = @At("TAIL"))
    private void onDrawScreenPost(int mouseX, int mouseY, float partialTicks, CallbackInfo ci)
    {
        if (Configs.Toggles.VILLAGER_TRADE_LIST.getBooleanValue() && this.widgetTradeList != null)
        {
            this.widgetTradeList.render(mouseX, mouseY, false);
        }
    }
}
