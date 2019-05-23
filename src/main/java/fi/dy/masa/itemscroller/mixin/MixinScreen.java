package fi.dy.masa.itemscroller.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.itemscroller.event.RenderEventHandler;
import net.minecraft.client.gui.AbstractParentElement;
import net.minecraft.client.gui.screen.Screen;

@Mixin(Screen.class)
public abstract class MixinScreen extends AbstractParentElement
{
    @Inject(method = "renderBackground()V", at = @At("RETURN"))
    protected void onDrawDefaultBackgroundPost(CallbackInfo ci)
    {
        RenderEventHandler.instance().onDrawBackgroundPost();
    }
}
