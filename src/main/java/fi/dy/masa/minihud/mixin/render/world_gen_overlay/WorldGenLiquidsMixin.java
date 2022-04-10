package fi.dy.masa.minihud.mixin.render.world_gen_overlay;

import java.util.Random;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;
import net.minecraft.block.Block;
import net.minecraft.init.Blocks;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraft.world.gen.feature.WorldGenLiquids;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.data.DataStorage;

@Mixin(WorldGenLiquids.class)
public abstract class WorldGenLiquidsMixin
{
    @Shadow @Final private Block block;

    @Inject(method = "generate", at = @At("HEAD"))
    private void onGenerate(World worldIn, Random rand, BlockPos position, CallbackInfoReturnable<Boolean> cir)
    {
        if (RendererToggle.WATER_FALLS.isRendererEnabled() &&
            this.block == Blocks.FLOWING_WATER)
        {
            DataStorage.getInstance().addWaterFallPosition(position);
        }
    }
}
