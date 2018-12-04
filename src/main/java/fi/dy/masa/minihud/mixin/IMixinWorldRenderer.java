package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Invoker;
import net.minecraft.client.renderer.WorldRenderer;

@Mixin(WorldRenderer.class)
public interface IMixinWorldRenderer
{
    @Invoker("getRenderedChunks")
    int getRenderedChunksInvoker();
}
