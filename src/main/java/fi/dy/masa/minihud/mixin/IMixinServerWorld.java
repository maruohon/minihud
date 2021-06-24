package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import net.minecraft.entity.Entity;
import net.minecraft.server.world.ServerEntityManager;
import net.minecraft.server.world.ServerWorld;

@Mixin(ServerWorld.class)
public interface IMixinServerWorld
{
    @Accessor("entityManager")
    ServerEntityManager<Entity> minihud_getEntityManager();
}
