package fi.dy.masa.itemscroller.mixin;

import javax.annotation.Nullable;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;
import net.minecraft.client.gui.screen.ingame.HandledScreen;
import net.minecraft.client.gui.screen.ingame.MerchantScreen;
import net.minecraft.client.util.math.MatrixStack;
import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.screen.MerchantScreenHandler;
import net.minecraft.text.Text;
import net.minecraft.village.TradeOffer;
import fi.dy.masa.itemscroller.config.Configs;
import fi.dy.masa.itemscroller.config.Hotkeys;
import fi.dy.masa.itemscroller.gui.ItemScrollerIcons;
import fi.dy.masa.itemscroller.util.InventoryUtils;
import fi.dy.masa.itemscroller.villager.FavoriteData;
import fi.dy.masa.itemscroller.villager.IMerchantScreenHandler;
import fi.dy.masa.itemscroller.villager.VillagerData;
import fi.dy.masa.itemscroller.villager.VillagerDataStorage;
import fi.dy.masa.itemscroller.villager.VillagerUtils;
import fi.dy.masa.malilib.gui.interfaces.IGuiIcon;
import fi.dy.masa.malilib.render.RenderUtils;

@Mixin(MerchantScreen.class)
public abstract class MixinMerchantScreen extends HandledScreen<MerchantScreenHandler>
{
    @Shadow private int selectedIndex;
    @Shadow private int indexStartOffset;

    @Nullable private FavoriteData favoriteData;
    private int indexStartOffsetLast = -1;

    private MixinMerchantScreen(MerchantScreenHandler handler, PlayerInventory inventory, Text title)
    {
        super(handler, inventory, title);
    }

    @Inject(method = "init", at = @At("RETURN"))
    private void initTradeListWidget(CallbackInfo ci)
    {
        if (Configs.Toggles.VILLAGER_TRADE_FEATURES.getBooleanValue() &&
            Configs.Generic.VILLAGER_TRADE_LIST_REMEMBER_SCROLL.getBooleanValue())
        {
            VillagerData data = VillagerDataStorage.getInstance().getDataForLastInteractionTarget();

            if (data != null)
            {
                this.indexStartOffset = data.getTradeListPosition();
            }
        }
    }

    @Inject(method = "mouseScrolled", at = @At("RETURN"))
    private void onMouseScrollPost(double mouseX, double mouseY, double amount, CallbackInfoReturnable<Boolean> cir)
    {
        if (Configs.Toggles.VILLAGER_TRADE_FEATURES.getBooleanValue() &&
            Configs.Generic.VILLAGER_TRADE_LIST_REMEMBER_SCROLL.getBooleanValue() &&
            this.indexStartOffsetLast != this.indexStartOffset)
        {
            VillagerDataStorage.getInstance().setTradeListPosition(this.indexStartOffset);
            this.indexStartOffsetLast = this.indexStartOffset;
        }
    }

    @Inject(method = "mouseDragged", at = @At("RETURN"))
    private void onMouseDragPost(double mouseX, double mouseY, int button, double deltaX, double deltaY, CallbackInfoReturnable<Boolean> cir)
    {
        if (Configs.Toggles.VILLAGER_TRADE_FEATURES.getBooleanValue() &&
            Configs.Generic.VILLAGER_TRADE_LIST_REMEMBER_SCROLL.getBooleanValue() &&
            this.indexStartOffsetLast != this.indexStartOffset)
        {
            VillagerDataStorage.getInstance().setTradeListPosition(this.indexStartOffset);
            this.indexStartOffsetLast = this.indexStartOffset;
        }
    }

    @Inject(method = "mouseClicked", at = @At("RETURN"), cancellable = true)
    private void onMouseClicked(double mouseX, double mouseY, int button, CallbackInfoReturnable<Boolean> cir)
    {
        if (Configs.Toggles.VILLAGER_TRADE_FEATURES.getBooleanValue())
        {
            int visibleIndex = this.getHoveredTradeButtonIndex(mouseX, mouseY);
            int realIndex = VillagerUtils.getRealTradeIndexFor(visibleIndex, this.handler);

            if (realIndex >= 0)
            {
                // right click, trade everything with this trade
                if (button == 1)
                {
                    InventoryUtils.villagerTradeEverythingPossibleWithTrade(visibleIndex);
                    cir.setReturnValue(true);
                }
                // Middle click, toggle trade favorite
                else if (button == 2)
                {
                    if (Hotkeys.MODIFIER_TOGGLE_VILLAGER_GLOBAL_FAVORITE.getKeybind().isKeybindHeld())
                    {
                        TradeOffer trade = this.handler.getRecipes().get(visibleIndex);
                        VillagerDataStorage.getInstance().toggleGlobalFavorite(trade);
                    }
                    else
                    {
                        VillagerDataStorage.getInstance().toggleFavorite(realIndex);
                    }

                    this.favoriteData = null; // Force a re-build of the list

                    // Rebuild the custom list based on the new favorites (See the Mixin for MerchantScreenHandler#setOffers())
                    this.handler.setOffers(((IMerchantScreenHandler) this.handler).getOriginalList());

                    cir.setReturnValue(true);
                }
            }
        }
    }

    @Inject(method = "syncRecipeIndex", at = @At("HEAD"), cancellable = true)
    private void fixRecipeIndex(CallbackInfo ci)
    {
        if (Configs.Toggles.VILLAGER_TRADE_FEATURES.getBooleanValue() &&
            this.getScreenHandler() instanceof IMerchantScreenHandler)
        {
            if (VillagerUtils.switchToTradeByVisibleIndex(this.selectedIndex))
            {
                ci.cancel();
            }
        }
    }

    @Inject(method = "render", at = @At(value = "FIELD",
            target = "Lnet/minecraft/client/gui/screen/ingame/MerchantScreen;offers:[Lnet/minecraft/client/gui/screen/ingame/MerchantScreen$WidgetButtonPage;"))
    private void renderFavoriteMarker(MatrixStack matrices, int mouseX, int mouseY, float delta, CallbackInfo ci)
    {
        if (Configs.Toggles.VILLAGER_TRADE_FEATURES.getBooleanValue())
        {
            FavoriteData favoriteData = this.favoriteData;

            if (favoriteData == null)
            {
                favoriteData = VillagerDataStorage.getInstance().getFavoritesForCurrentVillager(this.handler);
                this.favoriteData = favoriteData;
            }

            int numFavorites = favoriteData.favorites.size();

            if (numFavorites > 0 && this.indexStartOffset < numFavorites)
            {
                int screenX = (this.width - this.backgroundWidth) / 2;
                int screenY = (this.height - this.backgroundHeight) / 2;
                int buttonsStartX = screenX + 5;
                int buttonsStartY = screenY + 16 + 2;
                int x = buttonsStartX + 89 - 8;
                int y = buttonsStartY + 2;
                float z = this.getZOffset() + 300;
                IGuiIcon icon = favoriteData.isGlobal ? ItemScrollerIcons.STAR_5_PURPLE : ItemScrollerIcons.STAR_5_YELLOW;

                for (int i = 0; i < (numFavorites - this.indexStartOffset); ++i)
                {
                    RenderUtils.bindTexture(icon.getTexture());
                    icon.renderAt(x, y, z, false, false);
                    y += 20;
                }
            }
        }
    }

    private int getHoveredTradeButtonIndex(double mouseX, double mouseY)
    {
        int screenX = (this.width - this.backgroundWidth) / 2;
        int screenY = (this.height - this.backgroundHeight) / 2;
        int buttonsStartX = screenX + 5;
        int buttonsStartY = screenY + 16 + 2;
        int buttonWidth = 89;
        int buttonHeight = 20;

        if (mouseX >= buttonsStartX && mouseX <= buttonsStartX + buttonWidth &&
            mouseY >= buttonsStartY && mouseY <= buttonsStartY + 7 * buttonHeight)
        {
            return this.indexStartOffset + (((int) mouseY - buttonsStartY) / buttonHeight);
        }

        return -1;
    }
}
