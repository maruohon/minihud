package fi.dy.masa.minihud.mixin;

import java.util.List;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import net.minecraft.world.World;
import net.minecraft.world.chunk.BlockEntityTickInvoker;

@Mixin(World.class)
public interface IMixinWorld
{
    @Accessor("blockEntityTickers")
    List<BlockEntityTickInvoker> minihud_getBlockEntityTickers();
}
