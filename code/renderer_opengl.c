/* /////////////////////////////////////////////////////////////////////////// */
/* (c) copyright 2022 Lawrence D. Kern /////////////////////////////////////// */
/* /////////////////////////////////////////////////////////////////////////// */

// NOTE(law): Prefix any typedef'ed OpenGL function pointers with
// "opengl_function_" to make them uniformly accessible using the macros defined
// below.

#define DECLARE_OPENGL_FUNCTION(name) opengl_function_##name *name

#define LINUX_LOAD_OPENGL_FUNCTION(name) name = (opengl_function_##name *) \
   glXGetProcAddressARB((const GLubyte *)#name)

// TODO(law): Add any more OpenGL functions we need to the corresponding
// sections below.

// IMPORTANT(law): Any additions made to the OPENGL_FUNCTION_POINTERS list
// below must have a corresponding entry in the list of typedef'ed function
// prototypes, and vice versa.

#define OPENGL_FUNCTION_POINTERS \
   X(glCreateProgram)            \
   X(glLinkProgram)              \
   X(glUseProgram)               \
   X(glGetProgramiv)             \
   X(glGetProgramInfoLog)        \
   X(glCreateShader)             \
   X(glCompileShader)            \
   X(glAttachShader)             \
   X(glDetachShader)             \
   X(glDeleteShader)             \
   X(glShaderSource)             \
   X(glGetShaderiv)              \
   X(glGetShaderInfoLog)         \
   X(glValidateProgram)          \
   X(glVertexAttribPointer)      \
   X(glEnableVertexAttribArray)  \
   X(glDisableVertexAttribArray) \
   X(glGenBuffers)               \
   X(glGenVertexArrays)          \
   X(glBindBuffer)               \
   X(glBindVertexArray)          \
   X(glBufferData)               \

typedef GLuint opengl_function_glCreateProgram(void);
typedef   void opengl_function_glLinkProgram(GLuint program);
typedef   void opengl_function_glUseProgram(GLuint program);
typedef   void opengl_function_glGetProgramiv(GLuint program, GLenum pname, GLint *params);
typedef   void opengl_function_glGetProgramInfoLog(GLuint program, GLsizei maxLength, GLsizei *length, GLchar *infoLog);
typedef GLuint opengl_function_glCreateShader(GLenum shaderType);
typedef   void opengl_function_glCompileShader(GLuint shader);
typedef   void opengl_function_glAttachShader(GLuint program, GLuint shader);
typedef   void opengl_function_glDetachShader(GLuint program, GLuint shader);
typedef   void opengl_function_glDeleteShader(GLuint shader);
typedef   void opengl_function_glShaderSource(GLuint shader, GLsizei count, const GLchar **string, const GLint *length);
typedef   void opengl_function_glGetShaderiv(GLuint shader, GLenum pname, GLint *params);
typedef   void opengl_function_glGetShaderInfoLog(GLuint shader, GLsizei maxLength, GLsizei *length, GLchar *infoLog);
typedef   void opengl_function_glValidateProgram(GLuint program);
typedef   void opengl_function_glVertexAttribPointer(GLuint index, GLint size, GLenum type, GLboolean normalized, GLsizei stride, const GLvoid *pointer);
typedef   void opengl_function_glEnableVertexAttribArray(GLuint index);
typedef   void opengl_function_glDisableVertexAttribArray(GLuint index);
typedef   void opengl_function_glGenBuffers(GLsizei n, GLuint *buffers);
typedef   void opengl_function_glGenVertexArrays(GLsizei n, GLuint *arrays);
typedef   void opengl_function_glBindBuffer(GLenum target, GLuint buffer);
typedef   void opengl_function_glBindVertexArray(GLuint array);
typedef   void opengl_function_glBufferData(GLenum target, GLsizeiptr size, const GLvoid *data, GLenum usage);

#define X(name) DECLARE_OPENGL_FUNCTION(name);
   OPENGL_FUNCTION_POINTERS
#undef X

static GLuint opengl_global_vertex_buffer_object;
static GLuint opengl_global_vertex_array_object;
static GLuint opengl_global_shader_program;

static const char *vertex_shader_code =
"#version 330 core\n"
"\n"
"layout(location = 0) in vec2 position;\n"
"layout(location = 1) in vec2 vertex_texture_coordinate;\n"
"out vec2 fragment_texture_coordinate;\n"
"\n"
"void main()\n"
"{\n"
"   gl_Position = vec4(position, 0.0f, 1.0f);\n"
"   fragment_texture_coordinate = vertex_texture_coordinate;\n"
"}\n";

static const char *fragment_shader_code =
"#version 330 core\n"
"\n"
"in vec2 fragment_texture_coordinate;\n"
"out vec4 output_color;\n"
"uniform sampler2D bitmap_texture;\n"
"\n"
"void main()\n"
"{\n"
"   output_color = texture(bitmap_texture, fragment_texture_coordinate);\n"
"}\n";

static void
opengl_initialize()
{
   platform_log("=====\n");
   platform_log("OpenGL Information:\n");
   platform_log("Vendor: %s\n", glGetString(GL_VENDOR));
   platform_log("Renderer: %s\n", glGetString(GL_RENDERER));
   platform_log("Version: %s\n", glGetString(GL_VERSION));
   platform_log("Shading Language Version: %s\n", glGetString(GL_SHADING_LANGUAGE_VERSION));
   // platform_log("Extensions: %s\n", glGetString(GL_EXTENSIONS));
   platform_log("=====\n");

   // NOTE(law): Define the vertices of the bitmap we plan to blit.
   float vertices[] =
   {
      // NOTE(law): Lower triangle xy positions.
      +1, +1,
      +1, -1,
      -1, -1,

      // NOTE(law): Upper triangle xy positions.
      +1, +1,
      -1, -1,
      -1, +1,

      // NOTE(law): The source bitmap is top-down, so just reverse the y texture
      // coordinates for now to match the bottom-up convention of OpenGL.

      // NOTE(law): Lower triangle texture coordinates.
      1, 0,
      1, 1,
      0, 1,

      // NOTE(law): Upper triangle texture coordinates.
      1, 0,
      0, 1,
      0, 0,
   };

   glGenVertexArrays(1, &opengl_global_vertex_array_object);
   glBindVertexArray(opengl_global_vertex_array_object);
   {
      // NOTE(law): Set up vertex position buffer object.
      glGenBuffers(1, &opengl_global_vertex_buffer_object);
      glBindBuffer(GL_ARRAY_BUFFER, opengl_global_vertex_buffer_object);
      glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);
      glBindBuffer(GL_ARRAY_BUFFER, 0);

      glBindBuffer(GL_ARRAY_BUFFER, opengl_global_vertex_buffer_object);
      glEnableVertexAttribArray(0);
      glEnableVertexAttribArray(1);

      glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, 0);
      glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 0, (void *)48);

      // glDisableVertexAttribArray(0);
      // glDisableVertexAttribArray(1);
   }

   // NOTE(law): Compile vertex shader.
   GLuint vertex_shader = glCreateShader(GL_VERTEX_SHADER);
   glShaderSource(vertex_shader, 1, &vertex_shader_code, 0);
   glCompileShader(vertex_shader);

   GLint vertex_compilation_status;
   glGetShaderiv(vertex_shader, GL_COMPILE_STATUS, &vertex_compilation_status);
   if(vertex_compilation_status == GL_FALSE)
   {
      GLchar log[PLATFORM_LOG_MAX_LENGTH];
      glGetShaderInfoLog(vertex_shader, PLATFORM_LOG_MAX_LENGTH, 0, log);

      platform_log("ERROR: Compilation error in vertex shader.\n");
      platform_log(log);
   }
   assert(vertex_compilation_status == GL_TRUE);

   // NOTE(law): Compile fragment shader.
   GLuint fragment_shader = glCreateShader(GL_FRAGMENT_SHADER);
   glShaderSource(fragment_shader, 1, &fragment_shader_code, 0);
   glCompileShader(fragment_shader);

   GLint fragment_compilation_status;
   glGetShaderiv(fragment_shader, GL_COMPILE_STATUS, &fragment_compilation_status);
   if(fragment_compilation_status == GL_FALSE)
   {
      GLchar log[PLATFORM_LOG_MAX_LENGTH];
      glGetShaderInfoLog(fragment_shader, PLATFORM_LOG_MAX_LENGTH, 0, log);

      platform_log("ERROR: Compilation error in fragment shader.\n");
      platform_log(log);
   }
   assert(fragment_compilation_status == GL_TRUE);

   // NOTE(law): Create shader program.
   opengl_global_shader_program = glCreateProgram();
   glAttachShader(opengl_global_shader_program, vertex_shader);
   glAttachShader(opengl_global_shader_program, fragment_shader);
   glLinkProgram(opengl_global_shader_program);

   GLint program_link_status;
   glGetProgramiv(opengl_global_shader_program, GL_LINK_STATUS, &program_link_status);
   if(program_link_status == GL_FALSE)
   {
      GLchar log[PLATFORM_LOG_MAX_LENGTH];
      glGetProgramInfoLog(opengl_global_shader_program, PLATFORM_LOG_MAX_LENGTH, 0, log);

      platform_log("ERROR: Linking error in shader program.\n");
      platform_log(log);
   }
   assert(program_link_status == GL_TRUE);

   GLint program_status;
   glValidateProgram(opengl_global_shader_program);
   glGetProgramiv(opengl_global_shader_program, GL_VALIDATE_STATUS, &program_status);
   if (program_status == GL_FALSE)
   {
      GLchar log[PLATFORM_LOG_MAX_LENGTH];
      glGetProgramInfoLog(opengl_global_shader_program, PLATFORM_LOG_MAX_LENGTH, 0, log);

      platform_log("ERROR: The linked shader program is invalid.\n");
      platform_log(log);
   }
   assert(program_status == GL_TRUE);

   // NOTE(law): Clean up the shaders once the program has been created.
   glDetachShader(opengl_global_shader_program, vertex_shader);
   glDetachShader(opengl_global_shader_program, fragment_shader);

   glDeleteShader(vertex_shader);
   glDeleteShader(fragment_shader);
}

static void
opengl_display_bitmap(struct pixel_bitmap *bitmap, u32 client_width, u32 client_height)
{
   float client_aspect_ratio = (float)client_width / (float)client_height;
   float target_aspect_ratio = (float)RESOLUTION_BASE_WIDTH / (float)RESOLUTION_BASE_HEIGHT;

   float target_width  = (float)client_width;
   float target_height = (float)client_height;
   float gutter_width  = 0;
   float gutter_height = 0;

   if(client_aspect_ratio > target_aspect_ratio)
   {
      // NOTE(law): The window is too wide, fill in the left and right sides
      // with black gutters.
      target_width = target_aspect_ratio * (float)client_height;
      gutter_width = (client_width - target_width) / 2;
   }
   else if(client_aspect_ratio < target_aspect_ratio)
   {
      // NOTE(law): The window is too tall, fill in the top and bottom with
      // black gutters.
      target_height = (1.0f / target_aspect_ratio) * (float)client_width;
      gutter_height = (client_height - target_height) / 2;
   }

   // TODO(law): Should we only set the viewport on resize events?
   glViewport(gutter_width, gutter_height, target_width, target_height);

   // NOTE(law): Clear the window to black.
   glClearColor(0, 0, 0, 1);
   glClear(GL_COLOR_BUFFER_BIT);

   // NOTE(law): Set up the pixel bitmap as an OpenGL texture.
   glBindTexture(GL_TEXTURE_2D, 1);
   glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, bitmap->width, bitmap->height, 0,
                GL_BGRA_EXT, GL_UNSIGNED_BYTE, bitmap->memory);

   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_R, GL_CLAMP);

   glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
   glEnable(GL_TEXTURE_2D);

   // NOTE(law): Draw the bitmap using the previously-defined shaders.
   glUseProgram(opengl_global_shader_program);
   glBindVertexArray(opengl_global_vertex_array_object);

   glDrawArrays(GL_TRIANGLES, 0, 6);

   glBindVertexArray(0);
   glUseProgram(0);
}
