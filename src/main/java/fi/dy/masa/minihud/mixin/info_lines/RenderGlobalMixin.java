package fi.dy.masa.minihud.mixin.info_lines;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Invoker;
import net.minecraft.client.renderer.RenderGlobal;

@Mixin(RenderGlobal.class)
public interface RenderGlobalMixin
{
    @Invoker("getRenderedChunks")
    int minihud_getRenderedChunks();
}
