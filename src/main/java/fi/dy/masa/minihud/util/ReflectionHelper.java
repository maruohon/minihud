package fi.dy.masa.minihud.util;

import java.lang.reflect.Field;

public class ReflectionHelper
{
    public static class UnableToFindFieldException extends RuntimeException
    {
        private static final long serialVersionUID = 1L;

        public UnableToFindFieldException(String[] fieldNameList, Exception e)
        {
            super(e);
        }
    }

    public static Field findField(Class<?> clazz, String... fieldNames)
    {
        Exception failed = null;

        for (String fieldName : fieldNames)
        {
            try
            {
                Field f = clazz.getDeclaredField(fieldName);
                f.setAccessible(true);
                return f;
            }
            catch (Exception e)
            {
                failed = e;
            }
        }

        throw new UnableToFindFieldException(fieldNames, failed);
    }
}
