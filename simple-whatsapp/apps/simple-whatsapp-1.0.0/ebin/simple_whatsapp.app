{application, simple_whatsapp,
[{description, "A simple whatsapp like application"},
{vsn, "1.0.0"},
{env,[{port,8000}]},
{mod, {simple_whatsapp, []}},
{modules, [simple_whatsapp, chat_sup, chat_process, protocol]},
{applications, [stdlib, kernel,regis]}
]}.