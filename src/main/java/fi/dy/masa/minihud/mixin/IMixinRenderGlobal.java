package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Invoker;
import net.minecraft.client.renderer.RenderGlobal;

@Mixin(RenderGlobal.class)
public interface IMixinRenderGlobal
{
    @Invoker("getRenderedChunks")
    int getRenderedChunksInvoker();
}
