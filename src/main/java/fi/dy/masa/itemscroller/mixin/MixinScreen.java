package fi.dy.masa.itemscroller.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import net.minecraft.client.gui.AbstractParentElement;
import net.minecraft.client.gui.screen.Screen;
import fi.dy.masa.itemscroller.event.RenderEventHandler;

@Mixin(Screen.class)
public abstract class MixinScreen extends AbstractParentElement
{
    @Inject(method = "renderBackground(Lnet/minecraft/client/util/math/MatrixStack;)V", at = @At("RETURN"))
    protected void onDrawDefaultBackgroundPost(net.minecraft.client.util.math.MatrixStack matrixStack, CallbackInfo ci)
    {
        RenderEventHandler.instance().onDrawBackgroundPost(matrixStack);
    }
}
