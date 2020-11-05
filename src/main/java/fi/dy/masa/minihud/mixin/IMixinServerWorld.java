package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import net.minecraft.server.world.ServerWorld;

@Mixin(ServerWorld.class)
public interface IMixinServerWorld
{
    //@Accessor("entitiesById")
    //Int2ObjectMap<Entity> getEntityList();
}
