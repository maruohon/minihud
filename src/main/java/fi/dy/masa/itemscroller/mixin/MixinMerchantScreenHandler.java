package fi.dy.masa.itemscroller.mixin;

import org.jetbrains.annotations.Nullable;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;
import net.minecraft.screen.MerchantScreenHandler;
import net.minecraft.screen.ScreenHandler;
import net.minecraft.screen.ScreenHandlerType;
import net.minecraft.village.Merchant;
import net.minecraft.village.TradeOfferList;
import fi.dy.masa.itemscroller.config.Configs;
import fi.dy.masa.itemscroller.villager.IMerchantScreenHandler;
import fi.dy.masa.itemscroller.villager.VillagerUtils;

@Mixin(MerchantScreenHandler.class)
public abstract class MixinMerchantScreenHandler extends ScreenHandler implements IMerchantScreenHandler
{
    @Shadow @Final private Merchant merchant;
    @Nullable private TradeOfferList customList;

    protected MixinMerchantScreenHandler(@Nullable ScreenHandlerType<?> type, int syncId)
    {
        super(type, syncId);
    }

    @Inject(method = "getRecipes", at = @At("HEAD"), cancellable = true)
    private void replaceTradeList(CallbackInfoReturnable<TradeOfferList> cir)
    {
        if (Configs.Toggles.VILLAGER_TRADE_FEATURES.getBooleanValue() && this.customList != null)
        {
            cir.setReturnValue(this.customList);
        }
    }

    @Inject(method = "setOffers", at = @At("HEAD"))
    private void onTradeListSet(TradeOfferList offers, CallbackInfo ci)
    {
        if (Configs.Toggles.VILLAGER_TRADE_FEATURES.getBooleanValue())
        {
            this.customList = VillagerUtils.buildCustomTradeList(offers);
        }
    }

    @Override
    public TradeOfferList getOriginalList()
    {
        return this.merchant.getOffers();
    }
}
