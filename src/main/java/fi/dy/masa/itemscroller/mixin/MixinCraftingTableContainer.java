package fi.dy.masa.itemscroller.mixin;

import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.itemscroller.util.InventoryUtils;
import net.minecraft.container.CraftingTableContainer;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.inventory.CraftingInventory;
import net.minecraft.inventory.CraftingResultInventory;
import net.minecraft.inventory.Inventory;
import net.minecraft.world.World;

@Mixin(CraftingTableContainer.class)
public abstract class MixinCraftingTableContainer
{
    @Shadow @Final private CraftingInventory craftingInv;
    @Shadow @Final private CraftingResultInventory resultInv;
    @Shadow @Final private PlayerEntity player;

    @Inject(method = "onContentChanged", at = @At("RETURN"))
    private void onSlotChangedCraftingGrid(Inventory inventory, CallbackInfo ci)
    {
        InventoryUtils.onSlotChangedCraftingGrid(this.player, this.craftingInv, this.resultInv);
    }

    @Inject(method = "updateResult", at = @At("RETURN"))
    private static void onUpdateResult(int windowId, World world, PlayerEntity player,
            CraftingInventory craftingInv, CraftingResultInventory resultInv, CallbackInfo ci)
    {
        InventoryUtils.onSlotChangedCraftingGrid(player, craftingInv, resultInv);
    }
}
