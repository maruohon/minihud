package fi.dy.masa.itemscroller.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.itemscroller.util.InventoryUtils;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.inventory.Container;
import net.minecraft.inventory.InventoryCraftResult;
import net.minecraft.inventory.InventoryCrafting;
import net.minecraft.world.World;

@Mixin(Container.class)
public class MixinContainer
{
    //private static final String SCCG_SIG = "(Lnet/minecraft/world/World;Lnet/minecraft/entity/player/EntityPlayer;Lnet/minecraft/inventory/InventoryCrafting;Lnet/minecraft/inventory/InventoryCraftResult;)V";

    @Inject(method = "slotChangedCraftingGrid", at = @At("RETURN"))
    public void onSlotChangedCraftingGrid(
            World world,
            EntityPlayer player,
            InventoryCrafting inventoryCrafting,
            InventoryCraftResult inventoryCraftResult,
            CallbackInfo ci
    )
    {
        InventoryUtils.onSlotChangedCraftingGrid(world, player, inventoryCrafting, inventoryCraftResult);
    }
}
