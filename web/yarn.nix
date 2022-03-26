{ fetchurl, fetchgit, linkFarm, runCommand, gnutar }: rec {
  offline_cache = linkFarm "offline" packages;
  packages = [
    {
      name = "https___registry.npmjs.org__ampproject_remapping___remapping_2.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__ampproject_remapping___remapping_2.1.2.tgz";
        url  = "https://registry.npmjs.org/@ampproject/remapping/-/remapping-2.1.2.tgz";
        sha512 = "hoyByceqwKirw7w3Z7gnIIZC3Wx3J484Y3L/cMpXFbr7d9ZQj2mODrirNzcJa+SM3UlpWXYvKV4RlRpFXlWgXg==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_code_frame___code_frame_7.16.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_code_frame___code_frame_7.16.7.tgz";
        url  = "https://registry.npmjs.org/@babel/code-frame/-/code-frame-7.16.7.tgz";
        sha512 = "iAXqUn8IIeBTNd72xsFlgaXHkMBMt6y4HJp1tIaK465CWLT/fG1aqB7ykr95gHHmlBdGbFeWWfyB4NJJ0nmeIg==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_compat_data___compat_data_7.17.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_compat_data___compat_data_7.17.7.tgz";
        url  = "https://registry.npmjs.org/@babel/compat-data/-/compat-data-7.17.7.tgz";
        sha512 = "p8pdE6j0a29TNGebNm7NzYZWB3xVZJBZ7XGs42uAKzQo8VQ3F0By/cQCtUEABwIqw5zo6WA4NbmxsfzADzMKnQ==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_core___core_7.17.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_core___core_7.17.7.tgz";
        url  = "https://registry.npmjs.org/@babel/core/-/core-7.17.7.tgz";
        sha512 = "djHlEfFHnSnTAcPb7dATbiM5HxGOP98+3JLBZtjRb5I7RXrw7kFRoG2dXM8cm3H+o11A8IFH/uprmJpwFynRNQ==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_generator___generator_7.17.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_generator___generator_7.17.7.tgz";
        url  = "https://registry.npmjs.org/@babel/generator/-/generator-7.17.7.tgz";
        sha512 = "oLcVCTeIFadUoArDTwpluncplrYBmTCCZZgXCbgNGvOBBiSDDK3eWO4b/+eOTli5tKv1lg+a5/NAXg+nTcei1w==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_compilation_targets___helper_compilation_targets_7.17.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_compilation_targets___helper_compilation_targets_7.17.7.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-compilation-targets/-/helper-compilation-targets-7.17.7.tgz";
        sha512 = "UFzlz2jjd8kroj0hmCFV5zr+tQPi1dpC2cRsDV/3IEW8bJfCPrPpmcSN6ZS8RqIq4LXcmpipCQFPddyFA5Yc7w==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_environment_visitor___helper_environment_visitor_7.16.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_environment_visitor___helper_environment_visitor_7.16.7.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-environment-visitor/-/helper-environment-visitor-7.16.7.tgz";
        sha512 = "SLLb0AAn6PkUeAfKJCCOl9e1R53pQlGAfc4y4XuMRZfqeMYLE0dM1LMhqbGAlGQY0lfw5/ohoYWAe9V1yibRag==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_function_name___helper_function_name_7.16.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_function_name___helper_function_name_7.16.7.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-function-name/-/helper-function-name-7.16.7.tgz";
        sha512 = "QfDfEnIUyyBSR3HtrtGECuZ6DAyCkYFp7GHl75vFtTnn6pjKeK0T1DB5lLkFvBea8MdaiUABx3osbgLyInoejA==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_get_function_arity___helper_get_function_arity_7.16.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_get_function_arity___helper_get_function_arity_7.16.7.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-get-function-arity/-/helper-get-function-arity-7.16.7.tgz";
        sha512 = "flc+RLSOBXzNzVhcLu6ujeHUrD6tANAOU5ojrRx/as+tbzf8+stUCj7+IfRRoAbEZqj/ahXEMsjhOhgeZsrnTw==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_hoist_variables___helper_hoist_variables_7.16.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_hoist_variables___helper_hoist_variables_7.16.7.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-hoist-variables/-/helper-hoist-variables-7.16.7.tgz";
        sha512 = "m04d/0Op34H5v7pbZw6pSKP7weA6lsMvfiIAMeIvkY/R4xQtBSMFEigu9QTZ2qB/9l22vsxtM8a+Q8CzD255fg==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_module_imports___helper_module_imports_7.16.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_module_imports___helper_module_imports_7.16.7.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-module-imports/-/helper-module-imports-7.16.7.tgz";
        sha512 = "LVtS6TqjJHFc+nYeITRo6VLXve70xmq7wPhWTqDJusJEgGmkAACWwMiTNrvfoQo6hEhFwAIixNkvB0jPXDL8Wg==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_module_transforms___helper_module_transforms_7.17.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_module_transforms___helper_module_transforms_7.17.7.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-module-transforms/-/helper-module-transforms-7.17.7.tgz";
        sha512 = "VmZD99F3gNTYB7fJRDTi+u6l/zxY0BE6OIxPSU7a50s6ZUQkHwSDmV92FfM+oCG0pZRVojGYhkR8I0OGeCVREw==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_plugin_utils___helper_plugin_utils_7.16.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_plugin_utils___helper_plugin_utils_7.16.7.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-plugin-utils/-/helper-plugin-utils-7.16.7.tgz";
        sha512 = "Qg3Nk7ZxpgMrsox6HreY1ZNKdBq7K72tDSliA6dCl5f007jR4ne8iD5UzuNnCJH2xBf2BEEVGr+/OL6Gdp7RxA==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_simple_access___helper_simple_access_7.17.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_simple_access___helper_simple_access_7.17.7.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-simple-access/-/helper-simple-access-7.17.7.tgz";
        sha512 = "txyMCGroZ96i+Pxr3Je3lzEJjqwaRC9buMUgtomcrLe5Nd0+fk1h0LLA+ixUF5OW7AhHuQ7Es1WcQJZmZsz2XA==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_split_export_declaration___helper_split_export_declaration_7.16.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_split_export_declaration___helper_split_export_declaration_7.16.7.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-split-export-declaration/-/helper-split-export-declaration-7.16.7.tgz";
        sha512 = "xbWoy/PFoxSWazIToT9Sif+jJTlrMcndIsaOKvTA6u7QEo7ilkRZpjew18/W3c7nm8fXdUDXh02VXTbZ0pGDNw==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_validator_identifier___helper_validator_identifier_7.16.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_validator_identifier___helper_validator_identifier_7.16.7.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-validator-identifier/-/helper-validator-identifier-7.16.7.tgz";
        sha512 = "hsEnFemeiW4D08A5gUAZxLBTXpZ39P+a+DGDsHw1yxqyQ/jzFEnxf5uTEGp+3bzAbNOxU1paTgYS4ECU/IgfDw==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helper_validator_option___helper_validator_option_7.16.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helper_validator_option___helper_validator_option_7.16.7.tgz";
        url  = "https://registry.npmjs.org/@babel/helper-validator-option/-/helper-validator-option-7.16.7.tgz";
        sha512 = "TRtenOuRUVo9oIQGPC5G9DgK4743cdxvtOw0weQNpZXaS16SCBi5MNjZF8vba3ETURjZpTbVn7Vvcf2eAwFozQ==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_helpers___helpers_7.17.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_helpers___helpers_7.17.7.tgz";
        url  = "https://registry.npmjs.org/@babel/helpers/-/helpers-7.17.7.tgz";
        sha512 = "TKsj9NkjJfTBxM7Phfy7kv6yYc4ZcOo+AaWGqQOKTPDOmcGkIFb5xNA746eKisQkm4yavUYh4InYM9S+VnO01w==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_highlight___highlight_7.16.10.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_highlight___highlight_7.16.10.tgz";
        url  = "https://registry.npmjs.org/@babel/highlight/-/highlight-7.16.10.tgz";
        sha512 = "5FnTQLSLswEj6IkgVw5KusNUUFY9ZGqe/TRFnP/BKYHYgfh7tc+C7mwiy95/yNP7Dh9x580Vv8r7u7ZfTBFxdw==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_parser___parser_7.17.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_parser___parser_7.17.7.tgz";
        url  = "https://registry.npmjs.org/@babel/parser/-/parser-7.17.7.tgz";
        sha512 = "bm3AQf45vR4gKggRfvJdYJ0gFLoCbsPxiFLSH6hTVYABptNHY6l9NrhnucVjQ/X+SPtLANT9lc0fFhikj+VBRA==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_async_generators___plugin_syntax_async_generators_7.8.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_async_generators___plugin_syntax_async_generators_7.8.4.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-async-generators/-/plugin-syntax-async-generators-7.8.4.tgz";
        sha512 = "tycmZxkGfZaxhMRbXlPXuVFpdWlXpir2W4AMhSJgRKzk/eDlIXOhb2LHWoLpDF7TEHylV5zNhykX6KAgHJmTNw==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_bigint___plugin_syntax_bigint_7.8.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_bigint___plugin_syntax_bigint_7.8.3.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-bigint/-/plugin-syntax-bigint-7.8.3.tgz";
        sha512 = "wnTnFlG+YxQm3vDxpGE57Pj0srRU4sHE/mDkt1qv2YJJSeUAec2ma4WLUnUPeKjyrfntVwe/N6dCXpU+zL3Npg==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_class_properties___plugin_syntax_class_properties_7.12.13.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_class_properties___plugin_syntax_class_properties_7.12.13.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-class-properties/-/plugin-syntax-class-properties-7.12.13.tgz";
        sha512 = "fm4idjKla0YahUNgFNLCB0qySdsoPiZP3iQE3rky0mBUtMZ23yDJ9SJdg6dXTSDnulOVqiF3Hgr9nbXvXTQZYA==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_import_meta___plugin_syntax_import_meta_7.10.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_import_meta___plugin_syntax_import_meta_7.10.4.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-import-meta/-/plugin-syntax-import-meta-7.10.4.tgz";
        sha512 = "Yqfm+XDx0+Prh3VSeEQCPU81yC+JWZ2pDPFSS4ZdpfZhp4MkFMaDC1UqseovEKwSUpnIL7+vK+Clp7bfh0iD7g==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_json_strings___plugin_syntax_json_strings_7.8.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_json_strings___plugin_syntax_json_strings_7.8.3.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-json-strings/-/plugin-syntax-json-strings-7.8.3.tgz";
        sha512 = "lY6kdGpWHvjoe2vk4WrAapEuBR69EMxZl+RoGRhrFGNYVK8mOPAW8VfbT/ZgrFbXlDNiiaxQnAtgVCZ6jv30EA==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_logical_assignment_operators___plugin_syntax_logical_assignment_operators_7.10.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_logical_assignment_operators___plugin_syntax_logical_assignment_operators_7.10.4.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-logical-assignment-operators/-/plugin-syntax-logical-assignment-operators-7.10.4.tgz";
        sha512 = "d8waShlpFDinQ5MtvGU9xDAOzKH47+FFoney2baFIoMr952hKOLp1HR7VszoZvOsV/4+RRszNY7D17ba0te0ig==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_nullish_coalescing_operator___plugin_syntax_nullish_coalescing_operator_7.8.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_nullish_coalescing_operator___plugin_syntax_nullish_coalescing_operator_7.8.3.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-nullish-coalescing-operator/-/plugin-syntax-nullish-coalescing-operator-7.8.3.tgz";
        sha512 = "aSff4zPII1u2QD7y+F8oDsz19ew4IGEJg9SVW+bqwpwtfFleiQDMdzA/R+UlWDzfnHFCxxleFT0PMIrR36XLNQ==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_numeric_separator___plugin_syntax_numeric_separator_7.10.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_numeric_separator___plugin_syntax_numeric_separator_7.10.4.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-numeric-separator/-/plugin-syntax-numeric-separator-7.10.4.tgz";
        sha512 = "9H6YdfkcK/uOnY/K7/aA2xpzaAgkQn37yzWUMRK7OaPOqOpGS1+n0H5hxT9AUw9EsSjPW8SVyMJwYRtWs3X3ug==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_object_rest_spread___plugin_syntax_object_rest_spread_7.8.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_object_rest_spread___plugin_syntax_object_rest_spread_7.8.3.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-object-rest-spread/-/plugin-syntax-object-rest-spread-7.8.3.tgz";
        sha512 = "XoqMijGZb9y3y2XskN+P1wUGiVwWZ5JmoDRwx5+3GmEplNyVM2s2Dg8ILFQm8rWM48orGy5YpI5Bl8U1y7ydlA==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_optional_catch_binding___plugin_syntax_optional_catch_binding_7.8.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_optional_catch_binding___plugin_syntax_optional_catch_binding_7.8.3.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-optional-catch-binding/-/plugin-syntax-optional-catch-binding-7.8.3.tgz";
        sha512 = "6VPD0Pc1lpTqw0aKoeRTMiB+kWhAoT24PA+ksWSBrFtl5SIRVpZlwN3NNPQjehA2E/91FV3RjLWoVTglWcSV3Q==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_optional_chaining___plugin_syntax_optional_chaining_7.8.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_optional_chaining___plugin_syntax_optional_chaining_7.8.3.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-optional-chaining/-/plugin-syntax-optional-chaining-7.8.3.tgz";
        sha512 = "KoK9ErH1MBlCPxV0VANkXW2/dw4vlbGDrFgz8bmUsBGYkFRcbRwMh6cIJubdPrkxRwuGdtCk0v/wPTKbQgBjkg==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_top_level_await___plugin_syntax_top_level_await_7.14.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_top_level_await___plugin_syntax_top_level_await_7.14.5.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-top-level-await/-/plugin-syntax-top-level-await-7.14.5.tgz";
        sha512 = "hx++upLv5U1rgYfwe1xBQUhRmU41NEvpUvrp8jkrSCdvGSnM5/qdRMtylJ6PG5OFkBaHkbTAKTnd3/YyESRHFw==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_plugin_syntax_typescript___plugin_syntax_typescript_7.16.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_plugin_syntax_typescript___plugin_syntax_typescript_7.16.7.tgz";
        url  = "https://registry.npmjs.org/@babel/plugin-syntax-typescript/-/plugin-syntax-typescript-7.16.7.tgz";
        sha512 = "YhUIJHHGkqPgEcMYkPCKTyGUdoGKWtopIycQyjJH8OjvRgOYsXsaKehLVPScKJWAULPxMa4N1vCe6szREFlZ7A==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_template___template_7.16.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_template___template_7.16.7.tgz";
        url  = "https://registry.npmjs.org/@babel/template/-/template-7.16.7.tgz";
        sha512 = "I8j/x8kHUrbYRTUxXrrMbfCa7jxkE7tZre39x3kjr9hvI82cK1FfqLygotcWN5kdPGWcLdWMHpSBavse5tWw3w==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_traverse___traverse_7.17.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_traverse___traverse_7.17.3.tgz";
        url  = "https://registry.npmjs.org/@babel/traverse/-/traverse-7.17.3.tgz";
        sha512 = "5irClVky7TxRWIRtxlh2WPUUOLhcPN06AGgaQSB8AEwuyEBgJVuJ5imdHm5zxk8w0QS5T+tDfnDxAlhWjpb7cw==";
      };
    }
    {
      name = "https___registry.npmjs.org__babel_types___types_7.17.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__babel_types___types_7.17.0.tgz";
        url  = "https://registry.npmjs.org/@babel/types/-/types-7.17.0.tgz";
        sha512 = "TmKSNO4D5rzhL5bjWFcVHHLETzfQ/AmbKpKPOSjlP0WoHZ6L911fgoOKY4Alp/emzG4cHJdyN49zpgkbXFEHHw==";
      };
    }
    {
      name = "https___registry.npmjs.org__bcoe_v8_coverage___v8_coverage_0.2.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__bcoe_v8_coverage___v8_coverage_0.2.3.tgz";
        url  = "https://registry.npmjs.org/@bcoe/v8-coverage/-/v8-coverage-0.2.3.tgz";
        sha512 = "0hYQ8SB4Db5zvZB4axdMHGwEaQjkZzFjQiN9LVYvIFB2nSUHW9tYpxWriPrWDASIxiaXax83REcLxuSdnGPZtw==";
      };
    }
    {
      name = "https___registry.npmjs.org__codemirror_autocomplete___autocomplete_0.19.14.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__codemirror_autocomplete___autocomplete_0.19.14.tgz";
        url  = "https://registry.npmjs.org/@codemirror/autocomplete/-/autocomplete-0.19.14.tgz";
        sha512 = "4PqJG7GGTePc+FQF387RFebDV4ERvKj23gQBmzNtu64ZSHlYEGulwP5EIIfulBiaWEmei9TYVaMFmTdNfofpRQ==";
      };
    }
    {
      name = "https___registry.npmjs.org__codemirror_basic_setup___basic_setup_0.19.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__codemirror_basic_setup___basic_setup_0.19.1.tgz";
        url  = "https://registry.npmjs.org/@codemirror/basic-setup/-/basic-setup-0.19.1.tgz";
        sha512 = "gLjD7YgZU/we6BzS/ecCmD3viw83dsgv5ZUaSydYbYx9X4w4w9RqYnckcJ+0GDyHfNr5Jtfv2Z5ZtFQnBj0UDA==";
      };
    }
    {
      name = "https___registry.npmjs.org__codemirror_closebrackets___closebrackets_0.19.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__codemirror_closebrackets___closebrackets_0.19.1.tgz";
        url  = "https://registry.npmjs.org/@codemirror/closebrackets/-/closebrackets-0.19.1.tgz";
        sha512 = "ZiLXT6u+VuBK5QnfBbt/Vmfd9Pg6449wn1DIOWFZHUOldg5eFn3VGGjYY2XWuHQz5WuK+7dXamV2KE885O1gyA==";
      };
    }
    {
      name = "https___registry.npmjs.org__codemirror_commands___commands_0.19.8.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__codemirror_commands___commands_0.19.8.tgz";
        url  = "https://registry.npmjs.org/@codemirror/commands/-/commands-0.19.8.tgz";
        sha512 = "65LIMSGUGGpY3oH6mzV46YWRrgao6NmfJ+AuC7jNz3K5NPnH6GCV1H5I6SwOFyVbkiygGyd0EFwrWqywTBD1aw==";
      };
    }
    {
      name = "https___registry.npmjs.org__codemirror_comment___comment_0.19.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__codemirror_comment___comment_0.19.1.tgz";
        url  = "https://registry.npmjs.org/@codemirror/comment/-/comment-0.19.1.tgz";
        sha512 = "uGKteBuVWAC6fW+Yt8u27DOnXMT/xV4Ekk2Z5mRsiADCZDqYvryrJd6PLL5+8t64BVyocwQwNfz1UswYS2CtFQ==";
      };
    }
    {
      name = "https___registry.npmjs.org__codemirror_fold___fold_0.19.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__codemirror_fold___fold_0.19.3.tgz";
        url  = "https://registry.npmjs.org/@codemirror/fold/-/fold-0.19.3.tgz";
        sha512 = "8hT+Eq2G68mL0yPRvSD2ewhnLQAX6sbUJmtGVKFcj8oAXtfpYCX8LIcfXsuI19Qs7gZkOSpqZvn+KKj8IhZoAw==";
      };
    }
    {
      name = "https___registry.npmjs.org__codemirror_gutter___gutter_0.19.9.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__codemirror_gutter___gutter_0.19.9.tgz";
        url  = "https://registry.npmjs.org/@codemirror/gutter/-/gutter-0.19.9.tgz";
        sha512 = "PFrtmilahin1g6uL27aG5tM/rqR9DZzZYZsIrCXA5Uc2OFTFqx4owuhoU9hqfYxHp5ovfvBwQ+txFzqS4vog6Q==";
      };
    }
    {
      name = "https___registry.npmjs.org__codemirror_highlight___highlight_0.19.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__codemirror_highlight___highlight_0.19.7.tgz";
        url  = "https://registry.npmjs.org/@codemirror/highlight/-/highlight-0.19.7.tgz";
        sha512 = "3W32hBCY0pbbv/xidismw+RDMKuIag+fo4kZIbD7WoRj+Ttcaxjf+vP6RttRHXLaaqbWh031lTeON8kMlDhMYw==";
      };
    }
    {
      name = "https___registry.npmjs.org__codemirror_history___history_0.19.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__codemirror_history___history_0.19.2.tgz";
        url  = "https://registry.npmjs.org/@codemirror/history/-/history-0.19.2.tgz";
        sha512 = "unhP4t3N2smzmHoo/Yio6ueWi+il8gm9VKrvi6wlcdGH5fOfVDNkmjHQ495SiR+EdOG35+3iNebSPYww0vN7ow==";
      };
    }
    {
      name = "https___registry.npmjs.org__codemirror_language___language_0.19.8.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__codemirror_language___language_0.19.8.tgz";
        url  = "https://registry.npmjs.org/@codemirror/language/-/language-0.19.8.tgz";
        sha512 = "KhRne8qmzSKkaw+qhkwgNsPKxmThlyeJ3umfc33B9kJzVP7xhTkwX2MEPl0almM3brxMi+lPYx7gCPOy1gHsWw==";
      };
    }
    {
      name = "https___registry.npmjs.org__codemirror_lint___lint_0.19.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__codemirror_lint___lint_0.19.6.tgz";
        url  = "https://registry.npmjs.org/@codemirror/lint/-/lint-0.19.6.tgz";
        sha512 = "Pbw1Y5kHVs2J+itQ0uez3dI4qY9ApYVap7eNfV81x1/3/BXgBkKfadaw0gqJ4h4FDG7OnJwb0VbPsjJQllHjaA==";
      };
    }
    {
      name = "https___registry.npmjs.org__codemirror_matchbrackets___matchbrackets_0.19.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__codemirror_matchbrackets___matchbrackets_0.19.4.tgz";
        url  = "https://registry.npmjs.org/@codemirror/matchbrackets/-/matchbrackets-0.19.4.tgz";
        sha512 = "VFkaOKPNudAA5sGP1zikRHCEKU0hjYmkKpr04pybUpQvfTvNJXlReCyP0rvH/1iEwAGPL990ZTT+QrLdu4MeEA==";
      };
    }
    {
      name = "https___registry.npmjs.org__codemirror_panel___panel_0.19.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__codemirror_panel___panel_0.19.1.tgz";
        url  = "https://registry.npmjs.org/@codemirror/panel/-/panel-0.19.1.tgz";
        sha512 = "sYeOCMA3KRYxZYJYn5PNlt9yNsjy3zTNTrbYSfVgjgL9QomIVgOJWPO5hZ2sTN8lufO6lw0vTBsIPL9MSidmBg==";
      };
    }
    {
      name = "https___registry.npmjs.org__codemirror_rangeset___rangeset_0.19.9.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__codemirror_rangeset___rangeset_0.19.9.tgz";
        url  = "https://registry.npmjs.org/@codemirror/rangeset/-/rangeset-0.19.9.tgz";
        sha512 = "V8YUuOvK+ew87Xem+71nKcqu1SXd5QROMRLMS/ljT5/3MCxtgrRie1Cvild0G/Z2f1fpWxzX78V0U4jjXBorBQ==";
      };
    }
    {
      name = "https___registry.npmjs.org__codemirror_rectangular_selection___rectangular_selection_0.19.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__codemirror_rectangular_selection___rectangular_selection_0.19.1.tgz";
        url  = "https://registry.npmjs.org/@codemirror/rectangular-selection/-/rectangular-selection-0.19.1.tgz";
        sha512 = "9ElnqOg3mpZIWe0prPRd1SZ48Q9QB3bR8Aocq8UtjboJSUG8ABhRrbuTZMW/rMqpBPSjVpCe9xkCCkEQMYQVmw==";
      };
    }
    {
      name = "https___registry.npmjs.org__codemirror_search___search_0.19.9.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__codemirror_search___search_0.19.9.tgz";
        url  = "https://registry.npmjs.org/@codemirror/search/-/search-0.19.9.tgz";
        sha512 = "h3MuwbUbiyOp6Np3IB5r4LH0w4inZvbtLO1Ipmz8RhElcGRiYr11Q6Bim8ocLfe08RmZT6B5EkTj1E8eNlugQQ==";
      };
    }
    {
      name = "https___registry.npmjs.org__codemirror_state___state_0.19.9.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__codemirror_state___state_0.19.9.tgz";
        url  = "https://registry.npmjs.org/@codemirror/state/-/state-0.19.9.tgz";
        sha512 = "psOzDolKTZkx4CgUqhBQ8T8gBc0xN5z4gzed109aF6x7D7umpDRoimacI/O6d9UGuyl4eYuDCZmDFr2Rq7aGOw==";
      };
    }
    {
      name = "https___registry.npmjs.org__codemirror_text___text_0.19.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__codemirror_text___text_0.19.6.tgz";
        url  = "https://registry.npmjs.org/@codemirror/text/-/text-0.19.6.tgz";
        sha512 = "T9jnREMIygx+TPC1bOuepz18maGq/92q2a+n4qTqObKwvNMg+8cMTslb8yxeEDEq7S3kpgGWxgO1UWbQRij0dA==";
      };
    }
    {
      name = "https___registry.npmjs.org__codemirror_tooltip___tooltip_0.19.16.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__codemirror_tooltip___tooltip_0.19.16.tgz";
        url  = "https://registry.npmjs.org/@codemirror/tooltip/-/tooltip-0.19.16.tgz";
        sha512 = "zxKDHryUV5/RS45AQL+wOeN+i7/l81wK56OMnUPoTSzCWNITfxHn7BToDsjtrRKbzHqUxKYmBnn/4hPjpZ4WJQ==";
      };
    }
    {
      name = "https___registry.npmjs.org__codemirror_view___view_0.19.47.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__codemirror_view___view_0.19.47.tgz";
        url  = "https://registry.npmjs.org/@codemirror/view/-/view-0.19.47.tgz";
        sha512 = "SfbagKvJQl5dtt+9wYpo9sa3ZkMgUxTq+/hXDf0KVwIx+zu3cJIqfEm9xSx6yXkq7it7RsPGHaPasApNffF/8g==";
      };
    }
    {
      name = "https___registry.npmjs.org__cspotcode_source_map_consumer___source_map_consumer_0.8.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__cspotcode_source_map_consumer___source_map_consumer_0.8.0.tgz";
        url  = "https://registry.npmjs.org/@cspotcode/source-map-consumer/-/source-map-consumer-0.8.0.tgz";
        sha512 = "41qniHzTU8yAGbCp04ohlmSrZf8bkf/iJsl3V0dRGsQN/5GFfx+LbCSsCpp2gqrqjTVg/K6O8ycoV35JIwAzAg==";
      };
    }
    {
      name = "https___registry.npmjs.org__cspotcode_source_map_support___source_map_support_0.7.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__cspotcode_source_map_support___source_map_support_0.7.0.tgz";
        url  = "https://registry.npmjs.org/@cspotcode/source-map-support/-/source-map-support-0.7.0.tgz";
        sha512 = "X4xqRHqN8ACt2aHVe51OxeA2HjbcL4MqFqXkrmQszJ1NOUuUu5u6Vqx/0lZSVNku7velL5FC/s5uEAj1lsBMhA==";
      };
    }
    {
      name = "https___registry.npmjs.org__istanbuljs_load_nyc_config___load_nyc_config_1.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__istanbuljs_load_nyc_config___load_nyc_config_1.1.0.tgz";
        url  = "https://registry.npmjs.org/@istanbuljs/load-nyc-config/-/load-nyc-config-1.1.0.tgz";
        sha512 = "VjeHSlIzpv/NyD3N0YuHfXOPDIixcA1q2ZV98wsMqcYlPmv2n3Yb2lYP9XMElnaFVXg5A7YLTeLu6V84uQDjmQ==";
      };
    }
    {
      name = "https___registry.npmjs.org__istanbuljs_schema___schema_0.1.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__istanbuljs_schema___schema_0.1.3.tgz";
        url  = "https://registry.npmjs.org/@istanbuljs/schema/-/schema-0.1.3.tgz";
        sha512 = "ZXRY4jNvVgSVQ8DL3LTcakaAtXwTVUxE81hslsyD2AtoXW/wVob10HkOJ1X/pAlcI7D+2YoZKg5do8G/w6RYgA==";
      };
    }
    {
      name = "https___registry.npmjs.org__jest_console___console_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__jest_console___console_27.5.1.tgz";
        url  = "https://registry.npmjs.org/@jest/console/-/console-27.5.1.tgz";
        sha512 = "kZ/tNpS3NXn0mlXXXPNuDZnb4c0oZ20r4K5eemM2k30ZC3G0T02nXUvyhf5YdbXWHPEJLc9qGLxEZ216MdL+Zg==";
      };
    }
    {
      name = "https___registry.npmjs.org__jest_core___core_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__jest_core___core_27.5.1.tgz";
        url  = "https://registry.npmjs.org/@jest/core/-/core-27.5.1.tgz";
        sha512 = "AK6/UTrvQD0Cd24NSqmIA6rKsu0tKIxfiCducZvqxYdmMisOYAsdItspT+fQDQYARPf8XgjAFZi0ogW2agH5nQ==";
      };
    }
    {
      name = "https___registry.npmjs.org__jest_environment___environment_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__jest_environment___environment_27.5.1.tgz";
        url  = "https://registry.npmjs.org/@jest/environment/-/environment-27.5.1.tgz";
        sha512 = "/WQjhPJe3/ghaol/4Bq480JKXV/Rfw8nQdN7f41fM8VDHLcxKXou6QyXAh3EFr9/bVG3x74z1NWDkP87EiY8gA==";
      };
    }
    {
      name = "https___registry.npmjs.org__jest_fake_timers___fake_timers_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__jest_fake_timers___fake_timers_27.5.1.tgz";
        url  = "https://registry.npmjs.org/@jest/fake-timers/-/fake-timers-27.5.1.tgz";
        sha512 = "/aPowoolwa07k7/oM3aASneNeBGCmGQsc3ugN4u6s4C/+s5M64MFo/+djTdiwcbQlRfFElGuDXWzaWj6QgKObQ==";
      };
    }
    {
      name = "https___registry.npmjs.org__jest_globals___globals_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__jest_globals___globals_27.5.1.tgz";
        url  = "https://registry.npmjs.org/@jest/globals/-/globals-27.5.1.tgz";
        sha512 = "ZEJNB41OBQQgGzgyInAv0UUfDDj3upmHydjieSxFvTRuZElrx7tXg/uVQ5hYVEwiXs3+aMsAeEc9X7xiSKCm4Q==";
      };
    }
    {
      name = "https___registry.npmjs.org__jest_reporters___reporters_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__jest_reporters___reporters_27.5.1.tgz";
        url  = "https://registry.npmjs.org/@jest/reporters/-/reporters-27.5.1.tgz";
        sha512 = "cPXh9hWIlVJMQkVk84aIvXuBB4uQQmFqZiacloFuGiP3ah1sbCxCosidXFDfqG8+6fO1oR2dTJTlsOy4VFmUfw==";
      };
    }
    {
      name = "https___registry.npmjs.org__jest_source_map___source_map_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__jest_source_map___source_map_27.5.1.tgz";
        url  = "https://registry.npmjs.org/@jest/source-map/-/source-map-27.5.1.tgz";
        sha512 = "y9NIHUYF3PJRlHk98NdC/N1gl88BL08aQQgu4k4ZopQkCw9t9cV8mtl3TV8b/YCB8XaVTFrmUTAJvjsntDireg==";
      };
    }
    {
      name = "https___registry.npmjs.org__jest_test_result___test_result_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__jest_test_result___test_result_27.5.1.tgz";
        url  = "https://registry.npmjs.org/@jest/test-result/-/test-result-27.5.1.tgz";
        sha512 = "EW35l2RYFUcUQxFJz5Cv5MTOxlJIQs4I7gxzi2zVU7PJhOwfYq1MdC5nhSmYjX1gmMmLPvB3sIaC+BkcHRBfag==";
      };
    }
    {
      name = "https___registry.npmjs.org__jest_test_sequencer___test_sequencer_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__jest_test_sequencer___test_sequencer_27.5.1.tgz";
        url  = "https://registry.npmjs.org/@jest/test-sequencer/-/test-sequencer-27.5.1.tgz";
        sha512 = "LCheJF7WB2+9JuCS7VB/EmGIdQuhtqjRNI9A43idHv3E4KltCTsPsLxvdaubFHSYwY/fNjMWjl6vNRhDiN7vpQ==";
      };
    }
    {
      name = "https___registry.npmjs.org__jest_transform___transform_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__jest_transform___transform_27.5.1.tgz";
        url  = "https://registry.npmjs.org/@jest/transform/-/transform-27.5.1.tgz";
        sha512 = "ipON6WtYgl/1329g5AIJVbUuEh0wZVbdpGwC99Jw4LwuoBNS95MVphU6zOeD9pDkon+LLbFL7lOQRapbB8SCHw==";
      };
    }
    {
      name = "https___registry.npmjs.org__jest_types___types_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__jest_types___types_27.5.1.tgz";
        url  = "https://registry.npmjs.org/@jest/types/-/types-27.5.1.tgz";
        sha512 = "Cx46iJ9QpwQTjIdq5VJu2QTMMs3QlEjI0x1QbBP5W1+nMzyc2XmimiRR/CbX9TO0cPTeUlxWMOu8mslYsJ8DEw==";
      };
    }
    {
      name = "https___registry.npmjs.org__jridgewell_resolve_uri___resolve_uri_3.0.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__jridgewell_resolve_uri___resolve_uri_3.0.5.tgz";
        url  = "https://registry.npmjs.org/@jridgewell/resolve-uri/-/resolve-uri-3.0.5.tgz";
        sha512 = "VPeQ7+wH0itvQxnG+lIzWgkysKIr3L9sslimFW55rHMdGu/qCQ5z5h9zq4gI8uBtqkpHhsF4Z/OwExufUCThew==";
      };
    }
    {
      name = "https___registry.npmjs.org__jridgewell_sourcemap_codec___sourcemap_codec_1.4.11.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__jridgewell_sourcemap_codec___sourcemap_codec_1.4.11.tgz";
        url  = "https://registry.npmjs.org/@jridgewell/sourcemap-codec/-/sourcemap-codec-1.4.11.tgz";
        sha512 = "Fg32GrJo61m+VqYSdRSjRXMjQ06j8YIYfcTqndLYVAaHmroZHLJZCydsWBOTDqXS2v+mjxohBWEMfg97GXmYQg==";
      };
    }
    {
      name = "https___registry.npmjs.org__jridgewell_trace_mapping___trace_mapping_0.3.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__jridgewell_trace_mapping___trace_mapping_0.3.4.tgz";
        url  = "https://registry.npmjs.org/@jridgewell/trace-mapping/-/trace-mapping-0.3.4.tgz";
        sha512 = "vFv9ttIedivx0ux3QSjhgtCVjPZd5l46ZOMDSCwnH1yUO2e964gO8LZGyv2QkqcgR6TnBU1v+1IFqmeoG+0UJQ==";
      };
    }
    {
      name = "https___registry.npmjs.org__lezer_common___common_0.15.11.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__lezer_common___common_0.15.11.tgz";
        url  = "https://registry.npmjs.org/@lezer/common/-/common-0.15.11.tgz";
        sha512 = "vv0nSdIaVCRcJ8rPuDdsrNVfBOYe/4Szr/LhF929XyDmBndLDuWiCCHooGlGlJfzELyO608AyDhVsuX/ZG36NA==";
      };
    }
    {
      name = "https___registry.npmjs.org__lezer_generator___generator_0.15.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__lezer_generator___generator_0.15.4.tgz";
        url  = "https://registry.npmjs.org/@lezer/generator/-/generator-0.15.4.tgz";
        sha512 = "9bBwU2TzKMBQ6OCEDevuMNWGOBKlkq5YIGEhjrz9pb3MLb+oYYR4dVFZ7ehwLcDoSecsSA7PdlAy0thJO5pt2w==";
      };
    }
    {
      name = "https___registry.npmjs.org__lezer_lr___lr_0.15.8.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__lezer_lr___lr_0.15.8.tgz";
        url  = "https://registry.npmjs.org/@lezer/lr/-/lr-0.15.8.tgz";
        sha512 = "bM6oE6VQZ6hIFxDNKk8bKPa14hqFrV07J/vHGOeiAbJReIaQXmkVb6xQu4MR+JBTLa5arGRyAAjJe1qaQt3Uvg==";
      };
    }
    {
      name = "https___registry.npmjs.org__nodelib_fs.scandir___fs.scandir_2.1.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__nodelib_fs.scandir___fs.scandir_2.1.5.tgz";
        url  = "https://registry.npmjs.org/@nodelib/fs.scandir/-/fs.scandir-2.1.5.tgz";
        sha512 = "vq24Bq3ym5HEQm2NKCr3yXDwjc7vTsEThRDnkp2DK9p1uqLR+DHurm/NOTo0KG7HYHU7eppKZj3MyqYuMBf62g==";
      };
    }
    {
      name = "https___registry.npmjs.org__nodelib_fs.stat___fs.stat_2.0.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__nodelib_fs.stat___fs.stat_2.0.5.tgz";
        url  = "https://registry.npmjs.org/@nodelib/fs.stat/-/fs.stat-2.0.5.tgz";
        sha512 = "RkhPPp2zrqDAQA/2jNhnztcPAlv64XdhIp7a7454A5ovI7Bukxgt7MX7udwAu3zg1DcpPU0rz3VV1SeaqvY4+A==";
      };
    }
    {
      name = "https___registry.npmjs.org__nodelib_fs.walk___fs.walk_1.2.8.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__nodelib_fs.walk___fs.walk_1.2.8.tgz";
        url  = "https://registry.npmjs.org/@nodelib/fs.walk/-/fs.walk-1.2.8.tgz";
        sha512 = "oGB+UxlgWcgQkgwo8GcEGwemoTFt3FIO9ababBmaGwXIoBKZ+GTy0pP185beGg7Llih/NSHSV2XAs1lnznocSg==";
      };
    }
    {
      name = "https___registry.npmjs.org__nota_lang_esbuild_lezer___esbuild_lezer_0.3.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__nota_lang_esbuild_lezer___esbuild_lezer_0.3.3.tgz";
        url  = "https://registry.npmjs.org/@nota-lang/esbuild-lezer/-/esbuild-lezer-0.3.3.tgz";
        sha512 = "pUmC+sI/mFafIZLhE9wSYWNIjufrClsHr6qOkqS9TnTjWQzLzeExLRmBEPwMBNx0qWwWqel0n842sRueDhmEHg==";
      };
    }
    {
      name = "https___registry.npmjs.org__sinonjs_commons___commons_1.8.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__sinonjs_commons___commons_1.8.3.tgz";
        url  = "https://registry.npmjs.org/@sinonjs/commons/-/commons-1.8.3.tgz";
        sha512 = "xkNcLAn/wZaX14RPlwizcKicDk9G3F8m2nU3L7Ukm5zBgTwiT0wsoFAHx9Jq56fJA1z/7uKGtCRu16sOUCLIHQ==";
      };
    }
    {
      name = "https___registry.npmjs.org__sinonjs_fake_timers___fake_timers_8.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__sinonjs_fake_timers___fake_timers_8.1.0.tgz";
        url  = "https://registry.npmjs.org/@sinonjs/fake-timers/-/fake-timers-8.1.0.tgz";
        sha512 = "OAPJUAtgeINhh/TAlUID4QTs53Njm7xzddaVlEs/SXwgtiD1tW22zAB/W1wdqfrpmikgaWQ9Fw6Ws+hsiRm5Vg==";
      };
    }
    {
      name = "https___registry.npmjs.org__tailwindcss_forms___forms_0.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__tailwindcss_forms___forms_0.5.0.tgz";
        url  = "https://registry.npmjs.org/@tailwindcss/forms/-/forms-0.5.0.tgz";
        sha512 = "KzWugryEBFkmoaYcBE18rs6gthWCFHHO7cAZm2/hv3hwD67AzwP7udSCa22E7R1+CEJL/FfhYsJWrc0b1aeSzw==";
      };
    }
    {
      name = "https___registry.npmjs.org__tootallnate_once___once_1.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__tootallnate_once___once_1.1.2.tgz";
        url  = "https://registry.npmjs.org/@tootallnate/once/-/once-1.1.2.tgz";
        sha512 = "RbzJvlNzmRq5c3O09UipeuXno4tA1FE6ikOjxZK0tuxVv3412l64l5t1W5pj4+rJq9vpkm/kwiR07aZXnsKPxw==";
      };
    }
    {
      name = "https___registry.npmjs.org__tsconfig_node10___node10_1.0.8.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__tsconfig_node10___node10_1.0.8.tgz";
        url  = "https://registry.npmjs.org/@tsconfig/node10/-/node10-1.0.8.tgz";
        sha512 = "6XFfSQmMgq0CFLY1MslA/CPUfhIL919M1rMsa5lP2P097N2Wd1sSX0tx1u4olM16fLNhtHZpRhedZJphNJqmZg==";
      };
    }
    {
      name = "https___registry.npmjs.org__tsconfig_node12___node12_1.0.9.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__tsconfig_node12___node12_1.0.9.tgz";
        url  = "https://registry.npmjs.org/@tsconfig/node12/-/node12-1.0.9.tgz";
        sha512 = "/yBMcem+fbvhSREH+s14YJi18sp7J9jpuhYByADT2rypfajMZZN4WQ6zBGgBKp53NKmqI36wFYDb3yaMPurITw==";
      };
    }
    {
      name = "https___registry.npmjs.org__tsconfig_node14___node14_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__tsconfig_node14___node14_1.0.1.tgz";
        url  = "https://registry.npmjs.org/@tsconfig/node14/-/node14-1.0.1.tgz";
        sha512 = "509r2+yARFfHHE7T6Puu2jjkoycftovhXRqW328PDXTVGKihlb1P8Z9mMZH04ebyajfRY7dedfGynlrFHJUQCg==";
      };
    }
    {
      name = "https___registry.npmjs.org__tsconfig_node16___node16_1.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__tsconfig_node16___node16_1.0.2.tgz";
        url  = "https://registry.npmjs.org/@tsconfig/node16/-/node16-1.0.2.tgz";
        sha512 = "eZxlbI8GZscaGS7kkc/trHTT5xgrjH3/1n2JDwusC9iahPKWMRvRjJSAN5mCXviuTGQ/lHnhvv8Q1YTpnfz9gA==";
      };
    }
    {
      name = "https___registry.npmjs.org__types_babel__core___babel__core_7.1.18.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_babel__core___babel__core_7.1.18.tgz";
        url  = "https://registry.npmjs.org/@types/babel__core/-/babel__core-7.1.18.tgz";
        sha512 = "S7unDjm/C7z2A2R9NzfKCK1I+BAALDtxEmsJBwlB3EzNfb929ykjL++1CK9LO++EIp2fQrC8O+BwjKvz6UeDyQ==";
      };
    }
    {
      name = "https___registry.npmjs.org__types_babel__generator___babel__generator_7.6.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_babel__generator___babel__generator_7.6.4.tgz";
        url  = "https://registry.npmjs.org/@types/babel__generator/-/babel__generator-7.6.4.tgz";
        sha512 = "tFkciB9j2K755yrTALxD44McOrk+gfpIpvC3sxHjRawj6PfnQxrse4Clq5y/Rq+G3mrBurMax/lG8Qn2t9mSsg==";
      };
    }
    {
      name = "https___registry.npmjs.org__types_babel__template___babel__template_7.4.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_babel__template___babel__template_7.4.1.tgz";
        url  = "https://registry.npmjs.org/@types/babel__template/-/babel__template-7.4.1.tgz";
        sha512 = "azBFKemX6kMg5Io+/rdGT0dkGreboUVR0Cdm3fz9QJWpaQGJRQXl7C+6hOTCZcMll7KFyEQpgbYI2lHdsS4U7g==";
      };
    }
    {
      name = "https___registry.npmjs.org__types_babel__traverse___babel__traverse_7.14.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_babel__traverse___babel__traverse_7.14.2.tgz";
        url  = "https://registry.npmjs.org/@types/babel__traverse/-/babel__traverse-7.14.2.tgz";
        sha512 = "K2waXdXBi2302XUdcHcR1jCeU0LL4TD9HRs/gk0N2Xvrht+G/BfJa4QObBQZfhMdxiCpV3COl5Nfq4uKTeTnJA==";
      };
    }
    {
      name = "https___registry.npmjs.org__types_graceful_fs___graceful_fs_4.1.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_graceful_fs___graceful_fs_4.1.5.tgz";
        url  = "https://registry.npmjs.org/@types/graceful-fs/-/graceful-fs-4.1.5.tgz";
        sha512 = "anKkLmZZ+xm4p8JWBf4hElkM4XR+EZeA2M9BAkkTldmcyDY4mbdIJnRghDJH3Ov5ooY7/UAoENtmdMSkaAd7Cw==";
      };
    }
    {
      name = "https___registry.npmjs.org__types_istanbul_lib_coverage___istanbul_lib_coverage_2.0.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_istanbul_lib_coverage___istanbul_lib_coverage_2.0.4.tgz";
        url  = "https://registry.npmjs.org/@types/istanbul-lib-coverage/-/istanbul-lib-coverage-2.0.4.tgz";
        sha512 = "z/QT1XN4K4KYuslS23k62yDIDLwLFkzxOuMplDtObz0+y7VqJCaO2o+SPwHCvLFZh7xazvvoor2tA/hPz9ee7g==";
      };
    }
    {
      name = "https___registry.npmjs.org__types_istanbul_lib_report___istanbul_lib_report_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_istanbul_lib_report___istanbul_lib_report_3.0.0.tgz";
        url  = "https://registry.npmjs.org/@types/istanbul-lib-report/-/istanbul-lib-report-3.0.0.tgz";
        sha512 = "plGgXAPfVKFoYfa9NpYDAkseG+g6Jr294RqeqcqDixSbU34MZVJRi/P+7Y8GDpzkEwLaGZZOpKIEmeVZNtKsrg==";
      };
    }
    {
      name = "https___registry.npmjs.org__types_istanbul_reports___istanbul_reports_3.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_istanbul_reports___istanbul_reports_3.0.1.tgz";
        url  = "https://registry.npmjs.org/@types/istanbul-reports/-/istanbul-reports-3.0.1.tgz";
        sha512 = "c3mAZEuK0lvBp8tmuL74XRKn1+y2dcwOUpH7x4WrF6gk1GIgiluDRgMYQtw2OFcBvAJWlt6ASU3tSqxp0Uu0Aw==";
      };
    }
    {
      name = "https___registry.npmjs.org__types_jest___jest_27.4.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_jest___jest_27.4.1.tgz";
        url  = "https://registry.npmjs.org/@types/jest/-/jest-27.4.1.tgz";
        sha512 = "23iPJADSmicDVrWk+HT58LMJtzLAnB2AgIzplQuq/bSrGaxCrlvRFjGbXmamnnk/mAmCdLStiGqggu28ocUyiw==";
      };
    }
    {
      name = "https___registry.npmjs.org__types_less___less_3.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_less___less_3.0.3.tgz";
        url  = "https://registry.npmjs.org/@types/less/-/less-3.0.3.tgz";
        sha512 = "1YXyYH83h6We1djyoUEqTlVyQtCfJAFXELSKW2ZRtjHD4hQ82CC4lvrv5D0l0FLcKBaiPbXyi3MpMsI9ZRgKsw==";
      };
    }
    {
      name = "https___registry.npmjs.org__types_node___node_17.0.21.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_node___node_17.0.21.tgz";
        url  = "https://registry.npmjs.org/@types/node/-/node-17.0.21.tgz";
        sha512 = "DBZCJbhII3r90XbQxI8Y9IjjiiOGlZ0Hr32omXIZvwwZ7p4DMMXGrKXVyPfuoBOri9XNtL0UK69jYIBIsRX3QQ==";
      };
    }
    {
      name = "https___registry.npmjs.org__types_parse_json___parse_json_4.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_parse_json___parse_json_4.0.0.tgz";
        url  = "https://registry.npmjs.org/@types/parse-json/-/parse-json-4.0.0.tgz";
        sha512 = "//oorEZjL6sbPcKUaCdIGlIUeH26mgzimjBB77G6XRgnDl/L5wOnpyBGRe/Mmf5CVW3PwEBE1NjiMZ/ssFh4wA==";
      };
    }
    {
      name = "https___registry.npmjs.org__types_prettier___prettier_2.4.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_prettier___prettier_2.4.4.tgz";
        url  = "https://registry.npmjs.org/@types/prettier/-/prettier-2.4.4.tgz";
        sha512 = "ReVR2rLTV1kvtlWFyuot+d1pkpG2Fw/XKE3PDAdj57rbM97ttSp9JZ2UsP+2EHTylra9cUf6JA7tGwW1INzUrA==";
      };
    }
    {
      name = "https___registry.npmjs.org__types_sass___sass_1.43.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_sass___sass_1.43.1.tgz";
        url  = "https://registry.npmjs.org/@types/sass/-/sass-1.43.1.tgz";
        sha512 = "BPdoIt1lfJ6B7rw35ncdwBZrAssjcwzI5LByIrYs+tpXlj/CAkuVdRsgZDdP4lq5EjyWzwxZCqAoFyHKFwp32g==";
      };
    }
    {
      name = "https___registry.npmjs.org__types_stack_utils___stack_utils_2.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_stack_utils___stack_utils_2.0.1.tgz";
        url  = "https://registry.npmjs.org/@types/stack-utils/-/stack-utils-2.0.1.tgz";
        sha512 = "Hl219/BT5fLAaz6NDkSuhzasy49dwQS/DSdu4MdggFB8zcXv7vflBI3xp7FEmkmdDkBUI2bPUNeMttp2knYdxw==";
      };
    }
    {
      name = "https___registry.npmjs.org__types_stylus___stylus_0.48.36.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_stylus___stylus_0.48.36.tgz";
        url  = "https://registry.npmjs.org/@types/stylus/-/stylus-0.48.36.tgz";
        sha512 = "7klEq45BUE8ZJWkYWy1E442DcCs0wi0FkFY1Tjr6EJ7edL77t9w/QmOwlkFumBMqHlatDBtrA2xgfRrGqkUkzg==";
      };
    }
    {
      name = "https___registry.npmjs.org__types_yargs_parser___yargs_parser_21.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_yargs_parser___yargs_parser_21.0.0.tgz";
        url  = "https://registry.npmjs.org/@types/yargs-parser/-/yargs-parser-21.0.0.tgz";
        sha512 = "iO9ZQHkZxHn4mSakYV0vFHAVDyEOIJQrV2uZ06HxEPcx+mt8swXoZHIbaaJ2crJYFfErySgktuTZ3BeLz+XmFA==";
      };
    }
    {
      name = "https___registry.npmjs.org__types_yargs___yargs_16.0.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org__types_yargs___yargs_16.0.4.tgz";
        url  = "https://registry.npmjs.org/@types/yargs/-/yargs-16.0.4.tgz";
        sha512 = "T8Yc9wt/5LbJyCaLiHPReJa0kApcIgJ7Bn735GjItUfh08Z1pJvu8QZqb9s+mMvKV6WUQRV7K2R46YbjMXTTJw==";
      };
    }
    {
      name = "https___registry.npmjs.org_abab___abab_2.0.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_abab___abab_2.0.5.tgz";
        url  = "https://registry.npmjs.org/abab/-/abab-2.0.5.tgz";
        sha512 = "9IK9EadsbHo6jLWIpxpR6pL0sazTXV6+SQv25ZB+F7Bj9mJNaOc4nCRabwd5M/JwmUa8idz6Eci6eKfJryPs6Q==";
      };
    }
    {
      name = "https___registry.npmjs.org_acorn_globals___acorn_globals_6.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_acorn_globals___acorn_globals_6.0.0.tgz";
        url  = "https://registry.npmjs.org/acorn-globals/-/acorn-globals-6.0.0.tgz";
        sha512 = "ZQl7LOWaF5ePqqcX4hLuv/bLXYQNfNWw2c0/yX/TsPRKamzHcTGQnlCjHT3TsmkOUVEPS3crCxiPfdzE/Trlhg==";
      };
    }
    {
      name = "https___registry.npmjs.org_acorn_node___acorn_node_1.8.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_acorn_node___acorn_node_1.8.2.tgz";
        url  = "https://registry.npmjs.org/acorn-node/-/acorn-node-1.8.2.tgz";
        sha512 = "8mt+fslDufLYntIoPAaIMUe/lrbrehIiwmR3t2k9LljIzoigEPF27eLk2hy8zSGzmR/ogr7zbRKINMo1u0yh5A==";
      };
    }
    {
      name = "https___registry.npmjs.org_acorn_walk___acorn_walk_7.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_acorn_walk___acorn_walk_7.2.0.tgz";
        url  = "https://registry.npmjs.org/acorn-walk/-/acorn-walk-7.2.0.tgz";
        sha512 = "OPdCF6GsMIP+Az+aWfAAOEt2/+iVDKE7oy6lJ098aoe59oAmK76qV6Gw60SbZ8jHuG2wH058GF4pLFbYamYrVA==";
      };
    }
    {
      name = "https___registry.npmjs.org_acorn_walk___acorn_walk_8.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_acorn_walk___acorn_walk_8.2.0.tgz";
        url  = "https://registry.npmjs.org/acorn-walk/-/acorn-walk-8.2.0.tgz";
        sha512 = "k+iyHEuPgSw6SbuDpGQM+06HQUa04DZ3o+F6CSzXMvvI5KMvnaEqXe+YVe555R9nn6GPt404fos4wcgpw12SDA==";
      };
    }
    {
      name = "https___registry.npmjs.org_acorn___acorn_7.4.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_acorn___acorn_7.4.1.tgz";
        url  = "https://registry.npmjs.org/acorn/-/acorn-7.4.1.tgz";
        sha512 = "nQyp0o1/mNdbTO1PO6kHkwSrmgZ0MT/jCCpNiwbUjGoRN4dlBhqJtoQuCnEOKzgTVwg0ZWiCoQy6SxMebQVh8A==";
      };
    }
    {
      name = "https___registry.npmjs.org_acorn___acorn_8.7.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_acorn___acorn_8.7.0.tgz";
        url  = "https://registry.npmjs.org/acorn/-/acorn-8.7.0.tgz";
        sha512 = "V/LGr1APy+PXIwKebEWrkZPwoeoF+w1jiOBUmuxuiUIaOHtob8Qc9BTrYo7VuI5fR8tqsy+buA2WFooR5olqvQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_agent_base___agent_base_6.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_agent_base___agent_base_6.0.2.tgz";
        url  = "https://registry.npmjs.org/agent-base/-/agent-base-6.0.2.tgz";
        sha512 = "RZNwNclF7+MS/8bDg70amg32dyeZGZxiDuQmZxKLAlQjr3jGyLx+4Kkk58UO7D2QdgFIQCovuSuZESne6RG6XQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_ansi_escapes___ansi_escapes_4.3.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ansi_escapes___ansi_escapes_4.3.2.tgz";
        url  = "https://registry.npmjs.org/ansi-escapes/-/ansi-escapes-4.3.2.tgz";
        sha512 = "gKXj5ALrKWQLsYG9jlTRmR/xKluxHV+Z9QEwNIgCfM1/uwPMCuzVVnh5mwTd+OuBZcwSIMbqssNWRm1lE51QaQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_ansi_regex___ansi_regex_5.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ansi_regex___ansi_regex_5.0.1.tgz";
        url  = "https://registry.npmjs.org/ansi-regex/-/ansi-regex-5.0.1.tgz";
        sha512 = "quJQXlTSUGL2LH9SUXo8VwsY4soanhgo6LNSm84E1LBcE8s3O0wpdiRzyR9z/ZZJMlMWv37qOOb9pdJlMUEKFQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_ansi_styles___ansi_styles_3.2.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ansi_styles___ansi_styles_3.2.1.tgz";
        url  = "https://registry.npmjs.org/ansi-styles/-/ansi-styles-3.2.1.tgz";
        sha512 = "VT0ZI6kZRdTh8YyJw3SMbYm/u+NqfsAxEpWO0Pf9sq8/e94WxxOpPKx9FR1FlyCtOVDNOQ+8ntlqFxiRc+r5qA==";
      };
    }
    {
      name = "https___registry.npmjs.org_ansi_styles___ansi_styles_4.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ansi_styles___ansi_styles_4.3.0.tgz";
        url  = "https://registry.npmjs.org/ansi-styles/-/ansi-styles-4.3.0.tgz";
        sha512 = "zbB9rCJAT1rbjiVDb2hqKFHNYLxgtk8NURxZ3IZwD3F6NtxbXZQCnnSi1Lkx+IDohdPlFp222wVALIheZJQSEg==";
      };
    }
    {
      name = "https___registry.npmjs.org_ansi_styles___ansi_styles_5.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ansi_styles___ansi_styles_5.2.0.tgz";
        url  = "https://registry.npmjs.org/ansi-styles/-/ansi-styles-5.2.0.tgz";
        sha512 = "Cxwpt2SfTzTtXcfOlzGEee8O+c+MmUgGrNiBcXnuWxuFJHe6a5Hz7qwhwe5OgaSYI0IJvkLqWX1ASG+cJOkEiA==";
      };
    }
    {
      name = "https___registry.npmjs.org_anymatch___anymatch_3.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_anymatch___anymatch_3.1.2.tgz";
        url  = "https://registry.npmjs.org/anymatch/-/anymatch-3.1.2.tgz";
        sha512 = "P43ePfOAIupkguHUycrc4qJ9kz8ZiuOUijaETwX7THt0Y/GNK7v0aa8rY816xWjZ7rJdA5XdMcpVFTKMq+RvWg==";
      };
    }
    {
      name = "https___registry.npmjs.org_arg___arg_4.1.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_arg___arg_4.1.3.tgz";
        url  = "https://registry.npmjs.org/arg/-/arg-4.1.3.tgz";
        sha512 = "58S9QDqG0Xx27YwPSt9fJxivjYl432YCwfDMfZ+71RAqUrZef7LrKQZ3LHLOwCS4FLNBplP533Zx895SeOCHvA==";
      };
    }
    {
      name = "https___registry.npmjs.org_arg___arg_5.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_arg___arg_5.0.1.tgz";
        url  = "https://registry.npmjs.org/arg/-/arg-5.0.1.tgz";
        sha512 = "e0hDa9H2Z9AwFkk2qDlwhoMYE4eToKarchkQHovNdLTCYMHZHeRjI71crOh+dio4K6u1IcwubQqo79Ga4CyAQA==";
      };
    }
    {
      name = "https___registry.npmjs.org_argparse___argparse_1.0.10.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_argparse___argparse_1.0.10.tgz";
        url  = "https://registry.npmjs.org/argparse/-/argparse-1.0.10.tgz";
        sha512 = "o5Roy6tNG4SL/FOkCAN6RzjiakZS25RLYFrcMttJqbdd8BWrnA+fGz57iN5Pb06pvBGvl5gQ0B48dJlslXvoTg==";
      };
    }
    {
      name = "https___registry.npmjs.org_asynckit___asynckit_0.4.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_asynckit___asynckit_0.4.0.tgz";
        url  = "https://registry.npmjs.org/asynckit/-/asynckit-0.4.0.tgz";
        sha1 = "x57Zf380y48robyXkLzDZkdLS3k=";
      };
    }
    {
      name = "https___registry.npmjs.org_autoprefixer___autoprefixer_10.4.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_autoprefixer___autoprefixer_10.4.2.tgz";
        url  = "https://registry.npmjs.org/autoprefixer/-/autoprefixer-10.4.2.tgz";
        sha512 = "9fOPpHKuDW1w/0EKfRmVnxTDt8166MAnLI3mgZ1JCnhNtYWxcJ6Ud5CO/AVOZi/AvFa8DY9RTy3h3+tFBlrrdQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_babel_jest___babel_jest_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_babel_jest___babel_jest_27.5.1.tgz";
        url  = "https://registry.npmjs.org/babel-jest/-/babel-jest-27.5.1.tgz";
        sha512 = "cdQ5dXjGRd0IBRATiQ4mZGlGlRE8kJpjPOixdNRdT+m3UcNqmYWN6rK6nvtXYfY3D76cb8s/O1Ss8ea24PIwcg==";
      };
    }
    {
      name = "https___registry.npmjs.org_babel_plugin_istanbul___babel_plugin_istanbul_6.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_babel_plugin_istanbul___babel_plugin_istanbul_6.1.1.tgz";
        url  = "https://registry.npmjs.org/babel-plugin-istanbul/-/babel-plugin-istanbul-6.1.1.tgz";
        sha512 = "Y1IQok9821cC9onCx5otgFfRm7Lm+I+wwxOx738M/WLPZ9Q42m4IG5W0FNX8WLL2gYMZo3JkuXIH2DOpWM+qwA==";
      };
    }
    {
      name = "https___registry.npmjs.org_babel_plugin_jest_hoist___babel_plugin_jest_hoist_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_babel_plugin_jest_hoist___babel_plugin_jest_hoist_27.5.1.tgz";
        url  = "https://registry.npmjs.org/babel-plugin-jest-hoist/-/babel-plugin-jest-hoist-27.5.1.tgz";
        sha512 = "50wCwD5EMNW4aRpOwtqzyZHIewTYNxLA4nhB+09d8BIssfNfzBRhkBIHiaPv1Si226TQSvp8gxAJm2iY2qs2hQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_babel_preset_current_node_syntax___babel_preset_current_node_syntax_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_babel_preset_current_node_syntax___babel_preset_current_node_syntax_1.0.1.tgz";
        url  = "https://registry.npmjs.org/babel-preset-current-node-syntax/-/babel-preset-current-node-syntax-1.0.1.tgz";
        sha512 = "M7LQ0bxarkxQoN+vz5aJPsLBn77n8QgTFmo8WK0/44auK2xlCXrYcUxHFxgU7qW5Yzw/CjmLRK2uJzaCd7LvqQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_babel_preset_jest___babel_preset_jest_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_babel_preset_jest___babel_preset_jest_27.5.1.tgz";
        url  = "https://registry.npmjs.org/babel-preset-jest/-/babel-preset-jest-27.5.1.tgz";
        sha512 = "Nptf2FzlPCWYuJg41HBqXVT8ym6bXOevuCTbhxlUpjwtysGaIWFvDEjp4y+G7fl13FgOdjs7P/DmErqH7da0Ag==";
      };
    }
    {
      name = "https___registry.npmjs.org_balanced_match___balanced_match_1.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_balanced_match___balanced_match_1.0.2.tgz";
        url  = "https://registry.npmjs.org/balanced-match/-/balanced-match-1.0.2.tgz";
        sha512 = "3oSeUO0TMV67hN1AmbXsK4yaqU7tjiHlbxRDZOpH0KW9+CeX4bRAaX0Anxt0tx2MrpRpWwQaPwIlISEJhYU5Pw==";
      };
    }
    {
      name = "https___registry.npmjs.org_binary_extensions___binary_extensions_2.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_binary_extensions___binary_extensions_2.2.0.tgz";
        url  = "https://registry.npmjs.org/binary-extensions/-/binary-extensions-2.2.0.tgz";
        sha512 = "jDctJ/IVQbZoJykoeHbhXpOlNBqGNcwXJKJog42E5HDPUwQTSdjCHdihjj0DlnheQ7blbT6dHOafNAiS8ooQKA==";
      };
    }
    {
      name = "https___registry.npmjs.org_brace_expansion___brace_expansion_1.1.11.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_brace_expansion___brace_expansion_1.1.11.tgz";
        url  = "https://registry.npmjs.org/brace-expansion/-/brace-expansion-1.1.11.tgz";
        sha512 = "iCuPHDFgrHX7H2vEI/5xpz07zSHB00TpugqhmYtVmMO6518mCuRMoOYFldEBl0g187ufozdaHgWKcYFb61qGiA==";
      };
    }
    {
      name = "https___registry.npmjs.org_braces___braces_3.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_braces___braces_3.0.2.tgz";
        url  = "https://registry.npmjs.org/braces/-/braces-3.0.2.tgz";
        sha512 = "b8um+L1RzM3WDSzvhm6gIz1yfTbBt6YTlcEKAvsmqCZZFw46z626lVj9j1yEPW33H5H+lBQpZMP1k8l+78Ha0A==";
      };
    }
    {
      name = "https___registry.npmjs.org_browser_process_hrtime___browser_process_hrtime_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_browser_process_hrtime___browser_process_hrtime_1.0.0.tgz";
        url  = "https://registry.npmjs.org/browser-process-hrtime/-/browser-process-hrtime-1.0.0.tgz";
        sha512 = "9o5UecI3GhkpM6DrXr69PblIuWxPKk9Y0jHBRhdocZ2y7YECBFCsHm79Pr3OyR2AvjhDkabFJaDJMYRazHgsow==";
      };
    }
    {
      name = "https___registry.npmjs.org_browserslist___browserslist_4.20.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_browserslist___browserslist_4.20.0.tgz";
        url  = "https://registry.npmjs.org/browserslist/-/browserslist-4.20.0.tgz";
        sha512 = "bnpOoa+DownbciXj0jVGENf8VYQnE2LNWomhYuCsMmmx9Jd9lwq0WXODuwpSsp8AVdKM2/HorrzxAfbKvWTByQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_bs_logger___bs_logger_0.2.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_bs_logger___bs_logger_0.2.6.tgz";
        url  = "https://registry.npmjs.org/bs-logger/-/bs-logger-0.2.6.tgz";
        sha512 = "pd8DCoxmbgc7hyPKOvxtqNcjYoOsABPQdcCUjGp3d42VR2CX1ORhk2A87oqqu5R1kk+76nsxZupkmyd+MVtCog==";
      };
    }
    {
      name = "https___registry.npmjs.org_bser___bser_2.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_bser___bser_2.1.1.tgz";
        url  = "https://registry.npmjs.org/bser/-/bser-2.1.1.tgz";
        sha512 = "gQxTNE/GAfIIrmHLUE3oJyp5FO6HRBfhjnw4/wMmA63ZGDJnWBmgY/lyQBpnDUkGmAhbSe39tx2d/iTOAfglwQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_buffer_from___buffer_from_1.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_buffer_from___buffer_from_1.1.2.tgz";
        url  = "https://registry.npmjs.org/buffer-from/-/buffer-from-1.1.2.tgz";
        sha512 = "E+XQCRwSbaaiChtv6k6Dwgc+bx+Bs6vuKJHHl5kox/BaKbhiXzqQOwK4cO22yElGp2OCmjwVhT3HmxgyPGnJfQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_callsites___callsites_3.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_callsites___callsites_3.1.0.tgz";
        url  = "https://registry.npmjs.org/callsites/-/callsites-3.1.0.tgz";
        sha512 = "P8BjAsXvZS+VIDUI11hHCQEv74YT67YUi5JJFNWIqL235sBmjX4+qx9Muvls5ivyNENctx46xQLQ3aTuE7ssaQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_camelcase_css___camelcase_css_2.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_camelcase_css___camelcase_css_2.0.1.tgz";
        url  = "https://registry.npmjs.org/camelcase-css/-/camelcase-css-2.0.1.tgz";
        sha512 = "QOSvevhslijgYwRx6Rv7zKdMF8lbRmx+uQGx2+vDc+KI/eBnsy9kit5aj23AgGu3pa4t9AgwbnXWqS+iOY+2aA==";
      };
    }
    {
      name = "https___registry.npmjs.org_camelcase___camelcase_5.3.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_camelcase___camelcase_5.3.1.tgz";
        url  = "https://registry.npmjs.org/camelcase/-/camelcase-5.3.1.tgz";
        sha512 = "L28STB170nwWS63UjtlEOE3dldQApaJXZkOI1uMFfzf3rRuPegHaHesyee+YxQ+W6SvRDQV6UrdOdRiR153wJg==";
      };
    }
    {
      name = "https___registry.npmjs.org_camelcase___camelcase_6.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_camelcase___camelcase_6.3.0.tgz";
        url  = "https://registry.npmjs.org/camelcase/-/camelcase-6.3.0.tgz";
        sha512 = "Gmy6FhYlCY7uOElZUSbxo2UCDH8owEk996gkbrpsgGtrJLM3J7jGxl9Ic7Qwwj4ivOE5AWZWRMecDdF7hqGjFA==";
      };
    }
    {
      name = "https___registry.npmjs.org_caniuse_lite___caniuse_lite_1.0.30001317.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_caniuse_lite___caniuse_lite_1.0.30001317.tgz";
        url  = "https://registry.npmjs.org/caniuse-lite/-/caniuse-lite-1.0.30001317.tgz";
        sha512 = "xIZLh8gBm4dqNX0gkzrBeyI86J2eCjWzYAs40q88smG844YIrN4tVQl/RhquHvKEKImWWFIVh1Lxe5n1G/N+GQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_chalk___chalk_2.4.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_chalk___chalk_2.4.2.tgz";
        url  = "https://registry.npmjs.org/chalk/-/chalk-2.4.2.tgz";
        sha512 = "Mti+f9lpJNcwF4tWV8/OrTTtF1gZi+f8FqlyAdouralcFWFQWF2+NgCHShjkCb+IFBLq9buZwE1xckQU4peSuQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_chalk___chalk_4.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_chalk___chalk_4.1.2.tgz";
        url  = "https://registry.npmjs.org/chalk/-/chalk-4.1.2.tgz";
        sha512 = "oKnbhFyRIXpUuez8iBMmyEa4nbj4IOQyuhc/wy9kY7/WVPcwIO9VA668Pu8RkO7+0G76SLROeyw9CpQ061i4mA==";
      };
    }
    {
      name = "https___registry.npmjs.org_char_regex___char_regex_1.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_char_regex___char_regex_1.0.2.tgz";
        url  = "https://registry.npmjs.org/char-regex/-/char-regex-1.0.2.tgz";
        sha512 = "kWWXztvZ5SBQV+eRgKFeh8q5sLuZY2+8WUIzlxWVTg+oGwY14qylx1KbKzHd8P6ZYkAg0xyIDU9JMHhyJMZ1jw==";
      };
    }
    {
      name = "https___registry.npmjs.org_chokidar___chokidar_3.5.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_chokidar___chokidar_3.5.3.tgz";
        url  = "https://registry.npmjs.org/chokidar/-/chokidar-3.5.3.tgz";
        sha512 = "Dr3sfKRP6oTcjf2JmUmFJfeVMvXBdegxB0iVQ5eb2V10uFJUCAS8OByZdVAyVb8xXNz3GjjTgj9kLWsZTqE6kw==";
      };
    }
    {
      name = "https___registry.npmjs.org_ci_info___ci_info_3.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ci_info___ci_info_3.3.0.tgz";
        url  = "https://registry.npmjs.org/ci-info/-/ci-info-3.3.0.tgz";
        sha512 = "riT/3vI5YpVH6/qomlDnJow6TBee2PBKSEpx3O32EGPYbWGIRsIlGRms3Sm74wYE1JMo8RnO04Hb12+v1J5ICw==";
      };
    }
    {
      name = "https___registry.npmjs.org_cjs_module_lexer___cjs_module_lexer_1.2.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_cjs_module_lexer___cjs_module_lexer_1.2.2.tgz";
        url  = "https://registry.npmjs.org/cjs-module-lexer/-/cjs-module-lexer-1.2.2.tgz";
        sha512 = "cOU9usZw8/dXIXKtwa8pM0OTJQuJkxMN6w30csNRUerHfeQ5R6U3kkU/FtJeIf3M202OHfY2U8ccInBG7/xogA==";
      };
    }
    {
      name = "https___registry.npmjs.org_cliui___cliui_7.0.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_cliui___cliui_7.0.4.tgz";
        url  = "https://registry.npmjs.org/cliui/-/cliui-7.0.4.tgz";
        sha512 = "OcRE68cOsVMXp1Yvonl/fzkQOyjLSu/8bhPDfQt0e0/Eb283TKP20Fs2MqoPsr9SwA595rRCA+QMzYc9nBP+JQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_co___co_4.6.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_co___co_4.6.0.tgz";
        url  = "https://registry.npmjs.org/co/-/co-4.6.0.tgz";
        sha1 = "bqa989hTrlTMuOR7+gvz+QMfsYQ=";
      };
    }
    {
      name = "https___registry.npmjs.org_collect_v8_coverage___collect_v8_coverage_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_collect_v8_coverage___collect_v8_coverage_1.0.1.tgz";
        url  = "https://registry.npmjs.org/collect-v8-coverage/-/collect-v8-coverage-1.0.1.tgz";
        sha512 = "iBPtljfCNcTKNAto0KEtDfZ3qzjJvqE3aTGZsbhjSBlorqpXJlaWWtPO35D+ZImoC3KWejX64o+yPGxhWSTzfg==";
      };
    }
    {
      name = "https___registry.npmjs.org_color_convert___color_convert_1.9.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_color_convert___color_convert_1.9.3.tgz";
        url  = "https://registry.npmjs.org/color-convert/-/color-convert-1.9.3.tgz";
        sha512 = "QfAUtd+vFdAtFQcC8CCyYt1fYWxSqAiK2cSD6zDB8N3cpsEBAvRxp9zOGg6G/SHHJYAT88/az/IuDGALsNVbGg==";
      };
    }
    {
      name = "https___registry.npmjs.org_color_convert___color_convert_2.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_color_convert___color_convert_2.0.1.tgz";
        url  = "https://registry.npmjs.org/color-convert/-/color-convert-2.0.1.tgz";
        sha512 = "RRECPsj7iu/xb5oKYcsFHSppFNnsj/52OVTRKb4zP5onXwVF3zVmmToNcOfGC+CRDpfK/U584fMg38ZHCaElKQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_color_name___color_name_1.1.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_color_name___color_name_1.1.3.tgz";
        url  = "https://registry.npmjs.org/color-name/-/color-name-1.1.3.tgz";
        sha1 = "p9BVi9icQveV3UIyj3QIMcpTvCU=";
      };
    }
    {
      name = "https___registry.npmjs.org_color_name___color_name_1.1.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_color_name___color_name_1.1.4.tgz";
        url  = "https://registry.npmjs.org/color-name/-/color-name-1.1.4.tgz";
        sha512 = "dOy+3AuW3a2wNbZHIuMZpTcgjGuLU/uBL/ubcZF9OXbDo8ff4O8yVp5Bf0efS8uEoYo5q4Fx7dY9OgQGXgAsQA==";
      };
    }
    {
      name = "https___registry.npmjs.org_combined_stream___combined_stream_1.0.8.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_combined_stream___combined_stream_1.0.8.tgz";
        url  = "https://registry.npmjs.org/combined-stream/-/combined-stream-1.0.8.tgz";
        sha512 = "FQN4MRfuJeHf7cBbBMJFXhKSDq+2kAArBlmRBvcvFE5BB1HZKXtSFASDhdlz9zOYwxh8lDdnvmMOe/+5cdoEdg==";
      };
    }
    {
      name = "https___registry.npmjs.org_concat_map___concat_map_0.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_concat_map___concat_map_0.0.1.tgz";
        url  = "https://registry.npmjs.org/concat-map/-/concat-map-0.0.1.tgz";
        sha1 = "2Klr13/Wjfd5OnMDajug1UBdR3s=";
      };
    }
    {
      name = "https___registry.npmjs.org_convert_source_map___convert_source_map_1.8.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_convert_source_map___convert_source_map_1.8.0.tgz";
        url  = "https://registry.npmjs.org/convert-source-map/-/convert-source-map-1.8.0.tgz";
        sha512 = "+OQdjP49zViI/6i7nIJpA8rAl4sV/JdPfU9nZs3VqOwGIgizICvuN2ru6fMd+4llL0tar18UYJXfZ/TWtmhUjA==";
      };
    }
    {
      name = "https___registry.npmjs.org_cosmiconfig___cosmiconfig_7.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_cosmiconfig___cosmiconfig_7.0.1.tgz";
        url  = "https://registry.npmjs.org/cosmiconfig/-/cosmiconfig-7.0.1.tgz";
        sha512 = "a1YWNUV2HwGimB7dU2s1wUMurNKjpx60HxBB6xUM8Re+2s1g1IIfJvFR0/iCF+XHdE0GMTKTuLR32UQff4TEyQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_create_require___create_require_1.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_create_require___create_require_1.1.1.tgz";
        url  = "https://registry.npmjs.org/create-require/-/create-require-1.1.1.tgz";
        sha512 = "dcKFX3jn0MpIaXjisoRvexIJVEKzaq7z2rZKxf+MSr9TkdmHmsU4m2lcLojrj/FHl8mk5VxMmYA+ftRkP/3oKQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_crelt___crelt_1.0.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_crelt___crelt_1.0.5.tgz";
        url  = "https://registry.npmjs.org/crelt/-/crelt-1.0.5.tgz";
        sha512 = "+BO9wPPi+DWTDcNYhr/W90myha8ptzftZT+LwcmUbbok0rcP/fequmFYCw8NMoH7pkAZQzU78b3kYrlua5a9eA==";
      };
    }
    {
      name = "https___registry.npmjs.org_cross_spawn___cross_spawn_7.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_cross_spawn___cross_spawn_7.0.3.tgz";
        url  = "https://registry.npmjs.org/cross-spawn/-/cross-spawn-7.0.3.tgz";
        sha512 = "iRDPJKUPVEND7dHPO8rkbOnPpyDygcDFtWjpeWNCgy8WP2rXcxXL8TskReQl6OrB2G7+UJrags1q15Fudc7G6w==";
      };
    }
    {
      name = "https___registry.npmjs.org_cssesc___cssesc_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_cssesc___cssesc_3.0.0.tgz";
        url  = "https://registry.npmjs.org/cssesc/-/cssesc-3.0.0.tgz";
        sha512 = "/Tb/JcjK111nNScGob5MNtsntNM1aCNUDipB/TkwZFhyDrrE47SOx/18wF2bbjgc3ZzCSKW1T5nt5EbFoAz/Vg==";
      };
    }
    {
      name = "https___registry.npmjs.org_cssom___cssom_0.4.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_cssom___cssom_0.4.4.tgz";
        url  = "https://registry.npmjs.org/cssom/-/cssom-0.4.4.tgz";
        sha512 = "p3pvU7r1MyyqbTk+WbNJIgJjG2VmTIaB10rI93LzVPrmDJKkzKYMtxxyAvQXR/NS6otuzveI7+7BBq3SjBS2mw==";
      };
    }
    {
      name = "https___registry.npmjs.org_cssom___cssom_0.3.8.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_cssom___cssom_0.3.8.tgz";
        url  = "https://registry.npmjs.org/cssom/-/cssom-0.3.8.tgz";
        sha512 = "b0tGHbfegbhPJpxpiBPU2sCkigAqtM9O121le6bbOlgyV+NyGyCmVfJ6QW9eRjz8CpNfWEOYBIMIGRYkLwsIYg==";
      };
    }
    {
      name = "https___registry.npmjs.org_cssstyle___cssstyle_2.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_cssstyle___cssstyle_2.3.0.tgz";
        url  = "https://registry.npmjs.org/cssstyle/-/cssstyle-2.3.0.tgz";
        sha512 = "AZL67abkUzIuvcHqk7c09cezpGNcxUxU4Ioi/05xHk4DQeTkWmGYftIE6ctU6AEt+Gn4n1lDStOtj7FKycP71A==";
      };
    }
    {
      name = "https___registry.npmjs.org_cwd___cwd_0.10.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_cwd___cwd_0.10.0.tgz";
        url  = "https://registry.npmjs.org/cwd/-/cwd-0.10.0.tgz";
        sha1 = "FyQAaUBXwioTsM8WFix+S3p/5Wc=";
      };
    }
    {
      name = "https___registry.npmjs.org_data_urls___data_urls_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_data_urls___data_urls_2.0.0.tgz";
        url  = "https://registry.npmjs.org/data-urls/-/data-urls-2.0.0.tgz";
        sha512 = "X5eWTSXO/BJmpdIKCRuKUgSCgAN0OwliVK3yPKbwIWU1Tdw5BRajxlzMidvh+gwko9AfQ9zIj52pzF91Q3YAvQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_debug___debug_4.3.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_debug___debug_4.3.3.tgz";
        url  = "https://registry.npmjs.org/debug/-/debug-4.3.3.tgz";
        sha512 = "/zxw5+vh1Tfv+4Qn7a5nsbcJKPaSvCDhojn6FEl9vupwK2VCSDtEiEtqr8DFtzYFOdz63LBkxec7DYuc2jon6Q==";
      };
    }
    {
      name = "https___registry.npmjs.org_decimal.js___decimal.js_10.3.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_decimal.js___decimal.js_10.3.1.tgz";
        url  = "https://registry.npmjs.org/decimal.js/-/decimal.js-10.3.1.tgz";
        sha512 = "V0pfhfr8suzyPGOx3nmq4aHqabehUZn6Ch9kyFpV79TGDTWFmHqUqXdabR7QHqxzrYolF4+tVmJhUG4OURg5dQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_dedent___dedent_0.7.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_dedent___dedent_0.7.0.tgz";
        url  = "https://registry.npmjs.org/dedent/-/dedent-0.7.0.tgz";
        sha1 = "JJXduvbrh0q7Dhvp3yLS5aVEMmw=";
      };
    }
    {
      name = "https___registry.npmjs.org_deep_is___deep_is_0.1.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_deep_is___deep_is_0.1.4.tgz";
        url  = "https://registry.npmjs.org/deep-is/-/deep-is-0.1.4.tgz";
        sha512 = "oIPzksmTg4/MriiaYGO+okXDT7ztn/w3Eptv/+gSIdMdKsJo0u4CfYNFJPy+4SKMuCqGw2wxnA+URMg3t8a/bQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_deepmerge___deepmerge_4.2.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_deepmerge___deepmerge_4.2.2.tgz";
        url  = "https://registry.npmjs.org/deepmerge/-/deepmerge-4.2.2.tgz";
        sha512 = "FJ3UgI4gIl+PHZm53knsuSFpE+nESMr7M4v9QcgB7S63Kj/6WqMiFQJpBBYz1Pt+66bZpP3Q7Lye0Oo9MPKEdg==";
      };
    }
    {
      name = "https___registry.npmjs.org_defined___defined_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_defined___defined_1.0.0.tgz";
        url  = "https://registry.npmjs.org/defined/-/defined-1.0.0.tgz";
        sha1 = "yY2bzvdWdBiOEQlpFRGZ45sfppM=";
      };
    }
    {
      name = "https___registry.npmjs.org_delayed_stream___delayed_stream_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_delayed_stream___delayed_stream_1.0.0.tgz";
        url  = "https://registry.npmjs.org/delayed-stream/-/delayed-stream-1.0.0.tgz";
        sha1 = "3zrhmayt+31ECqrgsp4icrJOxhk=";
      };
    }
    {
      name = "https___registry.npmjs.org_detect_newline___detect_newline_3.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_detect_newline___detect_newline_3.1.0.tgz";
        url  = "https://registry.npmjs.org/detect-newline/-/detect-newline-3.1.0.tgz";
        sha512 = "TLz+x/vEXm/Y7P7wn1EJFNLxYpUD4TgMosxY6fAVJUnJMbupHBOncxyWUG9OpTaH9EBD7uFI5LfEgmMOc54DsA==";
      };
    }
    {
      name = "https___registry.npmjs.org_detective___detective_5.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_detective___detective_5.2.0.tgz";
        url  = "https://registry.npmjs.org/detective/-/detective-5.2.0.tgz";
        sha512 = "6SsIx+nUUbuK0EthKjv0zrdnajCCXVYGmbYYiYjFVpzcjwEs/JMDZ8tPRG29J/HhN56t3GJp2cGSWDRjjot8Pg==";
      };
    }
    {
      name = "https___registry.npmjs.org_didyoumean___didyoumean_1.2.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_didyoumean___didyoumean_1.2.2.tgz";
        url  = "https://registry.npmjs.org/didyoumean/-/didyoumean-1.2.2.tgz";
        sha512 = "gxtyfqMg7GKyhQmb056K7M3xszy/myH8w+B4RT+QXBQsvAOdc3XymqDDPHx1BgPgsdAA5SIifona89YtRATDzw==";
      };
    }
    {
      name = "https___registry.npmjs.org_diff_sequences___diff_sequences_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_diff_sequences___diff_sequences_27.5.1.tgz";
        url  = "https://registry.npmjs.org/diff-sequences/-/diff-sequences-27.5.1.tgz";
        sha512 = "k1gCAXAsNgLwEL+Y8Wvl+M6oEFj5bgazfZULpS5CneoPPXRaCCW7dm+q21Ky2VEE5X+VeRDBVg1Pcvvsr4TtNQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_diff___diff_4.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_diff___diff_4.0.2.tgz";
        url  = "https://registry.npmjs.org/diff/-/diff-4.0.2.tgz";
        sha512 = "58lmxKSA4BNyLz+HHMUzlOEpg09FV+ev6ZMe3vJihgdxzgcwZ8VoEEPmALCZG9LmqfVoNMMKpttIYTVG6uDY7A==";
      };
    }
    {
      name = "https___registry.npmjs.org_dlv___dlv_1.1.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_dlv___dlv_1.1.3.tgz";
        url  = "https://registry.npmjs.org/dlv/-/dlv-1.1.3.tgz";
        sha512 = "+HlytyjlPKnIG8XuRG8WvmBP8xs8P71y+SKKS6ZXWoEgLuePxtDoUEiH7WkdePWrQ5JBpE6aoVqfZfJUQkjXwA==";
      };
    }
    {
      name = "https___registry.npmjs.org_domexception___domexception_2.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_domexception___domexception_2.0.1.tgz";
        url  = "https://registry.npmjs.org/domexception/-/domexception-2.0.1.tgz";
        sha512 = "yxJ2mFy/sibVQlu5qHjOkf9J3K6zgmCxgJ94u2EdvDOV09H+32LtRswEcUsmUWN72pVLOEnTSRaIVVzVQgS0dg==";
      };
    }
    {
      name = "https___registry.npmjs.org_electron_to_chromium___electron_to_chromium_1.4.84.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_electron_to_chromium___electron_to_chromium_1.4.84.tgz";
        url  = "https://registry.npmjs.org/electron-to-chromium/-/electron-to-chromium-1.4.84.tgz";
        sha512 = "b+DdcyOiZtLXHdgEG8lncYJdxbdJWJvclPNMg0eLUDcSOSO876WA/pYjdSblUTd7eJdIs4YdIxHWGazx7UPSJw==";
      };
    }
    {
      name = "https___registry.npmjs.org_emittery___emittery_0.8.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_emittery___emittery_0.8.1.tgz";
        url  = "https://registry.npmjs.org/emittery/-/emittery-0.8.1.tgz";
        sha512 = "uDfvUjVrfGJJhymx/kz6prltenw1u7WrCg1oa94zYY8xxVpLLUu045LAT0dhDZdXG58/EpPL/5kA180fQ/qudg==";
      };
    }
    {
      name = "https___registry.npmjs.org_emoji_regex___emoji_regex_8.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_emoji_regex___emoji_regex_8.0.0.tgz";
        url  = "https://registry.npmjs.org/emoji-regex/-/emoji-regex-8.0.0.tgz";
        sha512 = "MSjYzcWNOA0ewAHpz0MxpYFvwg6yjy1NG3xteoqz644VCo/RPgnr1/GGt+ic3iJTzQ8Eu3TdM14SawnVUmGE6A==";
      };
    }
    {
      name = "https___registry.npmjs.org_error_ex___error_ex_1.3.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_error_ex___error_ex_1.3.2.tgz";
        url  = "https://registry.npmjs.org/error-ex/-/error-ex-1.3.2.tgz";
        sha512 = "7dFHNmqeFSEt2ZBsCriorKnn3Z2pj+fd9kmI6QoWw4//DL+icEBfc0U7qJCisqrTsKTjw4fNFy2pW9OqStD84g==";
      };
    }
    {
      name = "esbuild_android_64___esbuild_android_64_0.14.27.tgz";
      path = fetchurl {
        name = "esbuild_android_64___esbuild_android_64_0.14.27.tgz";
        url  = "https://registry.yarnpkg.com/esbuild-android-64/-/esbuild-android-64-0.14.27.tgz";
        sha512 = "LuEd4uPuj/16Y8j6kqy3Z2E9vNY9logfq8Tq+oTE2PZVuNs3M1kj5Qd4O95ee66yDGb3isaOCV7sOLDwtMfGaQ==";
      };
    }
    {
      name = "esbuild_android_arm64___esbuild_android_arm64_0.14.27.tgz";
      path = fetchurl {
        name = "esbuild_android_arm64___esbuild_android_arm64_0.14.27.tgz";
        url  = "https://registry.yarnpkg.com/esbuild-android-arm64/-/esbuild-android-arm64-0.14.27.tgz";
        sha512 = "E8Ktwwa6vX8q7QeJmg8yepBYXaee50OdQS3BFtEHKrzbV45H4foMOeEE7uqdjGQZFBap5VAqo7pvjlyA92wznQ==";
      };
    }
    {
      name = "esbuild_darwin_64___esbuild_darwin_64_0.14.27.tgz";
      path = fetchurl {
        name = "esbuild_darwin_64___esbuild_darwin_64_0.14.27.tgz";
        url  = "https://registry.yarnpkg.com/esbuild-darwin-64/-/esbuild-darwin-64-0.14.27.tgz";
        sha512 = "czw/kXl/1ZdenPWfw9jDc5iuIYxqUxgQ/Q+hRd4/3udyGGVI31r29LCViN2bAJgGvQkqyLGVcG03PJPEXQ5i2g==";
      };
    }
    {
      name = "esbuild_darwin_arm64___esbuild_darwin_arm64_0.14.27.tgz";
      path = fetchurl {
        name = "esbuild_darwin_arm64___esbuild_darwin_arm64_0.14.27.tgz";
        url  = "https://registry.yarnpkg.com/esbuild-darwin-arm64/-/esbuild-darwin-arm64-0.14.27.tgz";
        sha512 = "BEsv2U2U4o672oV8+xpXNxN9bgqRCtddQC6WBh4YhXKDcSZcdNh7+6nS+DM2vu7qWIWNA4JbRG24LUUYXysimQ==";
      };
    }
    {
      name = "esbuild_freebsd_64___esbuild_freebsd_64_0.14.27.tgz";
      path = fetchurl {
        name = "esbuild_freebsd_64___esbuild_freebsd_64_0.14.27.tgz";
        url  = "https://registry.yarnpkg.com/esbuild-freebsd-64/-/esbuild-freebsd-64-0.14.27.tgz";
        sha512 = "7FeiFPGBo+ga+kOkDxtPmdPZdayrSzsV9pmfHxcyLKxu+3oTcajeZlOO1y9HW+t5aFZPiv7czOHM4KNd0tNwCA==";
      };
    }
    {
      name = "esbuild_freebsd_arm64___esbuild_freebsd_arm64_0.14.27.tgz";
      path = fetchurl {
        name = "esbuild_freebsd_arm64___esbuild_freebsd_arm64_0.14.27.tgz";
        url  = "https://registry.yarnpkg.com/esbuild-freebsd-arm64/-/esbuild-freebsd-arm64-0.14.27.tgz";
        sha512 = "8CK3++foRZJluOWXpllG5zwAVlxtv36NpHfsbWS7TYlD8S+QruXltKlXToc/5ZNzBK++l6rvRKELu/puCLc7jA==";
      };
    }
    {
      name = "esbuild_linux_32___esbuild_linux_32_0.14.27.tgz";
      path = fetchurl {
        name = "esbuild_linux_32___esbuild_linux_32_0.14.27.tgz";
        url  = "https://registry.yarnpkg.com/esbuild-linux-32/-/esbuild-linux-32-0.14.27.tgz";
        sha512 = "qhNYIcT+EsYSBClZ5QhLzFzV5iVsP1YsITqblSaztr3+ZJUI+GoK8aXHyzKd7/CKKuK93cxEMJPpfi1dfsOfdw==";
      };
    }
    {
      name = "https___registry.npmjs.org_esbuild_linux_64___esbuild_linux_64_0.14.27.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_esbuild_linux_64___esbuild_linux_64_0.14.27.tgz";
        url  = "https://registry.npmjs.org/esbuild-linux-64/-/esbuild-linux-64-0.14.27.tgz";
        sha512 = "ESjck9+EsHoTaKWlFKJpPZRN26uiav5gkI16RuI8WBxUdLrrAlYuYSndxxKgEn1csd968BX/8yQZATYf/9+/qg==";
      };
    }
    {
      name = "esbuild_linux_arm64___esbuild_linux_arm64_0.14.27.tgz";
      path = fetchurl {
        name = "esbuild_linux_arm64___esbuild_linux_arm64_0.14.27.tgz";
        url  = "https://registry.yarnpkg.com/esbuild-linux-arm64/-/esbuild-linux-arm64-0.14.27.tgz";
        sha512 = "no6Mi17eV2tHlJnqBHRLekpZ2/VYx+NfGxKcBE/2xOMYwctsanCaXxw4zapvNrGE9X38vefVXLz6YCF8b1EHiQ==";
      };
    }
    {
      name = "esbuild_linux_arm___esbuild_linux_arm_0.14.27.tgz";
      path = fetchurl {
        name = "esbuild_linux_arm___esbuild_linux_arm_0.14.27.tgz";
        url  = "https://registry.yarnpkg.com/esbuild-linux-arm/-/esbuild-linux-arm-0.14.27.tgz";
        sha512 = "JnnmgUBdqLQO9hoNZQqNHFWlNpSX82vzB3rYuCJMhtkuaWQEmQz6Lec1UIxJdC38ifEghNTBsF9bbe8dFilnCw==";
      };
    }
    {
      name = "esbuild_linux_mips64le___esbuild_linux_mips64le_0.14.27.tgz";
      path = fetchurl {
        name = "esbuild_linux_mips64le___esbuild_linux_mips64le_0.14.27.tgz";
        url  = "https://registry.yarnpkg.com/esbuild-linux-mips64le/-/esbuild-linux-mips64le-0.14.27.tgz";
        sha512 = "NolWP2uOvIJpbwpsDbwfeExZOY1bZNlWE/kVfkzLMsSgqeVcl5YMen/cedRe9mKnpfLli+i0uSp7N+fkKNU27A==";
      };
    }
    {
      name = "esbuild_linux_ppc64le___esbuild_linux_ppc64le_0.14.27.tgz";
      path = fetchurl {
        name = "esbuild_linux_ppc64le___esbuild_linux_ppc64le_0.14.27.tgz";
        url  = "https://registry.yarnpkg.com/esbuild-linux-ppc64le/-/esbuild-linux-ppc64le-0.14.27.tgz";
        sha512 = "/7dTjDvXMdRKmsSxKXeWyonuGgblnYDn0MI1xDC7J1VQXny8k1qgNp6VmrlsawwnsymSUUiThhkJsI+rx0taNA==";
      };
    }
    {
      name = "esbuild_linux_riscv64___esbuild_linux_riscv64_0.14.27.tgz";
      path = fetchurl {
        name = "esbuild_linux_riscv64___esbuild_linux_riscv64_0.14.27.tgz";
        url  = "https://registry.yarnpkg.com/esbuild-linux-riscv64/-/esbuild-linux-riscv64-0.14.27.tgz";
        sha512 = "D+aFiUzOJG13RhrSmZgrcFaF4UUHpqj7XSKrIiCXIj1dkIkFqdrmqMSOtSs78dOtObWiOrFCDDzB24UyeEiNGg==";
      };
    }
    {
      name = "esbuild_linux_s390x___esbuild_linux_s390x_0.14.27.tgz";
      path = fetchurl {
        name = "esbuild_linux_s390x___esbuild_linux_s390x_0.14.27.tgz";
        url  = "https://registry.yarnpkg.com/esbuild-linux-s390x/-/esbuild-linux-s390x-0.14.27.tgz";
        sha512 = "CD/D4tj0U4UQjELkdNlZhQ8nDHU5rBn6NGp47Hiz0Y7/akAY5i0oGadhEIg0WCY/HYVXFb3CsSPPwaKcTOW3bg==";
      };
    }
    {
      name = "esbuild_netbsd_64___esbuild_netbsd_64_0.14.27.tgz";
      path = fetchurl {
        name = "esbuild_netbsd_64___esbuild_netbsd_64_0.14.27.tgz";
        url  = "https://registry.yarnpkg.com/esbuild-netbsd-64/-/esbuild-netbsd-64-0.14.27.tgz";
        sha512 = "h3mAld69SrO1VoaMpYl3a5FNdGRE/Nqc+E8VtHOag4tyBwhCQXxtvDDOAKOUQexBGca0IuR6UayQ4ntSX5ij1Q==";
      };
    }
    {
      name = "esbuild_openbsd_64___esbuild_openbsd_64_0.14.27.tgz";
      path = fetchurl {
        name = "esbuild_openbsd_64___esbuild_openbsd_64_0.14.27.tgz";
        url  = "https://registry.yarnpkg.com/esbuild-openbsd-64/-/esbuild-openbsd-64-0.14.27.tgz";
        sha512 = "xwSje6qIZaDHXWoPpIgvL+7fC6WeubHHv18tusLYMwL+Z6bEa4Pbfs5IWDtQdHkArtfxEkIZz77944z8MgDxGw==";
      };
    }
    {
      name = "https___registry.npmjs.org_esbuild_style_plugin___esbuild_style_plugin_1.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_esbuild_style_plugin___esbuild_style_plugin_1.3.0.tgz";
        url  = "https://registry.npmjs.org/esbuild-style-plugin/-/esbuild-style-plugin-1.3.0.tgz";
        sha512 = "8PqGlfRKd2a9ye5FMJpkCwoxL9Qd7Q+nO5xbju/Le6zwyuPVkTKtQ/aXTmNXepK7MYCfTT6SbC3ehxbLbGxqkQ==";
      };
    }
    {
      name = "esbuild_sunos_64___esbuild_sunos_64_0.14.27.tgz";
      path = fetchurl {
        name = "esbuild_sunos_64___esbuild_sunos_64_0.14.27.tgz";
        url  = "https://registry.yarnpkg.com/esbuild-sunos-64/-/esbuild-sunos-64-0.14.27.tgz";
        sha512 = "/nBVpWIDjYiyMhuqIqbXXsxBc58cBVH9uztAOIfWShStxq9BNBik92oPQPJ57nzWXRNKQUEFWr4Q98utDWz7jg==";
      };
    }
    {
      name = "esbuild_windows_32___esbuild_windows_32_0.14.27.tgz";
      path = fetchurl {
        name = "esbuild_windows_32___esbuild_windows_32_0.14.27.tgz";
        url  = "https://registry.yarnpkg.com/esbuild-windows-32/-/esbuild-windows-32-0.14.27.tgz";
        sha512 = "Q9/zEjhZJ4trtWhFWIZvS/7RUzzi8rvkoaS9oiizkHTTKd8UxFwn/Mm2OywsAfYymgUYm8+y2b+BKTNEFxUekw==";
      };
    }
    {
      name = "esbuild_windows_64___esbuild_windows_64_0.14.27.tgz";
      path = fetchurl {
        name = "esbuild_windows_64___esbuild_windows_64_0.14.27.tgz";
        url  = "https://registry.yarnpkg.com/esbuild-windows-64/-/esbuild-windows-64-0.14.27.tgz";
        sha512 = "b3y3vTSl5aEhWHK66ngtiS/c6byLf6y/ZBvODH1YkBM+MGtVL6jN38FdHUsZasCz9gFwYs/lJMVY9u7GL6wfYg==";
      };
    }
    {
      name = "esbuild_windows_arm64___esbuild_windows_arm64_0.14.27.tgz";
      path = fetchurl {
        name = "esbuild_windows_arm64___esbuild_windows_arm64_0.14.27.tgz";
        url  = "https://registry.yarnpkg.com/esbuild-windows-arm64/-/esbuild-windows-arm64-0.14.27.tgz";
        sha512 = "I/reTxr6TFMcR5qbIkwRGvldMIaiBu2+MP0LlD7sOlNXrfqIl9uNjsuxFPGEG4IRomjfQ5q8WT+xlF/ySVkqKg==";
      };
    }
    {
      name = "https___registry.npmjs.org_esbuild___esbuild_0.14.27.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_esbuild___esbuild_0.14.27.tgz";
        url  = "https://registry.npmjs.org/esbuild/-/esbuild-0.14.27.tgz";
        sha512 = "MZQt5SywZS3hA9fXnMhR22dv0oPGh6QtjJRIYbgL1AeqAoQZE+Qn5ppGYQAoHv/vq827flj4tIJ79Mrdiwk46Q==";
      };
    }
    {
      name = "https___registry.npmjs.org_escalade___escalade_3.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_escalade___escalade_3.1.1.tgz";
        url  = "https://registry.npmjs.org/escalade/-/escalade-3.1.1.tgz";
        sha512 = "k0er2gUkLf8O0zKJiAhmkTnJlTvINGv7ygDNPbeIsX/TJjGJZHuh9B2UxbsaEkmlEo9MfhrSzmhIlhRlI2GXnw==";
      };
    }
    {
      name = "https___registry.npmjs.org_escape_string_regexp___escape_string_regexp_1.0.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_escape_string_regexp___escape_string_regexp_1.0.5.tgz";
        url  = "https://registry.npmjs.org/escape-string-regexp/-/escape-string-regexp-1.0.5.tgz";
        sha1 = "G2HAViGQqN/2rjuyzwIAyhMLhtQ=";
      };
    }
    {
      name = "https___registry.npmjs.org_escape_string_regexp___escape_string_regexp_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_escape_string_regexp___escape_string_regexp_2.0.0.tgz";
        url  = "https://registry.npmjs.org/escape-string-regexp/-/escape-string-regexp-2.0.0.tgz";
        sha512 = "UpzcLCXolUWcNu5HtVMHYdXJjArjsF9C0aNnquZYY4uW/Vu0miy5YoWvbV345HauVvcAUnpRuhMMcqTcGOY2+w==";
      };
    }
    {
      name = "https___registry.npmjs.org_escodegen___escodegen_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_escodegen___escodegen_2.0.0.tgz";
        url  = "https://registry.npmjs.org/escodegen/-/escodegen-2.0.0.tgz";
        sha512 = "mmHKys/C8BFUGI+MAWNcSYoORYLMdPzjrknd2Vc+bUsjN5bXcr8EhrNB+UTqfL1y3I9c4fw2ihgtMPQLBRiQxw==";
      };
    }
    {
      name = "https___registry.npmjs.org_esprima___esprima_4.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_esprima___esprima_4.0.1.tgz";
        url  = "https://registry.npmjs.org/esprima/-/esprima-4.0.1.tgz";
        sha512 = "eGuFFw7Upda+g4p+QHvnW0RyTX/SVeJBDM/gCtMARO0cLuT2HcEKnTPvhjV6aGeqrCB/sbNop0Kszm0jsaWU4A==";
      };
    }
    {
      name = "https___registry.npmjs.org_estraverse___estraverse_5.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_estraverse___estraverse_5.3.0.tgz";
        url  = "https://registry.npmjs.org/estraverse/-/estraverse-5.3.0.tgz";
        sha512 = "MMdARuVEQziNTeJD8DgMqmhwR11BRQ/cBP+pLtYdSTnf3MIO8fFeiINEbX36ZdNlfU/7A9f3gUw49B3oQsvwBA==";
      };
    }
    {
      name = "https___registry.npmjs.org_esutils___esutils_2.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_esutils___esutils_2.0.3.tgz";
        url  = "https://registry.npmjs.org/esutils/-/esutils-2.0.3.tgz";
        sha512 = "kVscqXk4OCp68SZ0dkgEKVi6/8ij300KBWTJq32P/dYeWTSwK41WyTxalN1eRmA5Z9UU/LX9D7FWSmV9SAYx6g==";
      };
    }
    {
      name = "https___registry.npmjs.org_execa___execa_5.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_execa___execa_5.1.1.tgz";
        url  = "https://registry.npmjs.org/execa/-/execa-5.1.1.tgz";
        sha512 = "8uSpZZocAZRBAPIEINJj3Lo9HyGitllczc27Eh5YYojjMFMn8yHMDMaUHE2Jqfq05D/wucwI4JGURyXt1vchyg==";
      };
    }
    {
      name = "https___registry.npmjs.org_exit___exit_0.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_exit___exit_0.1.2.tgz";
        url  = "https://registry.npmjs.org/exit/-/exit-0.1.2.tgz";
        sha1 = "BjJjj42HfMghB9MKD/8aF8uhzQw=";
      };
    }
    {
      name = "https___registry.npmjs.org_expand_tilde___expand_tilde_1.2.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_expand_tilde___expand_tilde_1.2.2.tgz";
        url  = "https://registry.npmjs.org/expand-tilde/-/expand-tilde-1.2.2.tgz";
        sha1 = "C4HrqJflo9MdHD0QL48BRB5VlEk=";
      };
    }
    {
      name = "https___registry.npmjs.org_expand_tilde___expand_tilde_2.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_expand_tilde___expand_tilde_2.0.2.tgz";
        url  = "https://registry.npmjs.org/expand-tilde/-/expand-tilde-2.0.2.tgz";
        sha1 = "l+gBqgUt8CRU3kawK/YhZCzchQI=";
      };
    }
    {
      name = "https___registry.npmjs.org_expect___expect_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_expect___expect_27.5.1.tgz";
        url  = "https://registry.npmjs.org/expect/-/expect-27.5.1.tgz";
        sha512 = "E1q5hSUG2AmYQwQJ041nvgpkODHQvB+RKlB4IYdru6uJsyFTRyZAP463M+1lINorwbqAmUggi6+WwkD8lCS/Dw==";
      };
    }
    {
      name = "https___registry.npmjs.org_extend_shallow___extend_shallow_2.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_extend_shallow___extend_shallow_2.0.1.tgz";
        url  = "https://registry.npmjs.org/extend-shallow/-/extend-shallow-2.0.1.tgz";
        sha1 = "Ua99YUrZqfYQ6huvu5idaxxWiQ8=";
      };
    }
    {
      name = "https___registry.npmjs.org_fast_glob___fast_glob_3.2.11.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_fast_glob___fast_glob_3.2.11.tgz";
        url  = "https://registry.npmjs.org/fast-glob/-/fast-glob-3.2.11.tgz";
        sha512 = "xrO3+1bxSo3ZVHAnqzyuewYT6aMFHRAd4Kcs92MAonjwQZLsK9d0SF1IyQ3k5PoirxTW0Oe/RqFgMQ6TcNE5Ew==";
      };
    }
    {
      name = "https___registry.npmjs.org_fast_json_stable_stringify___fast_json_stable_stringify_2.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_fast_json_stable_stringify___fast_json_stable_stringify_2.1.0.tgz";
        url  = "https://registry.npmjs.org/fast-json-stable-stringify/-/fast-json-stable-stringify-2.1.0.tgz";
        sha512 = "lhd/wF+Lk98HZoTCtlVraHtfh5XYijIjalXck7saUtuanSDyLMxnHhSXEDJqHxD7msR8D0uCmqlkwjCV8xvwHw==";
      };
    }
    {
      name = "https___registry.npmjs.org_fast_levenshtein___fast_levenshtein_2.0.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_fast_levenshtein___fast_levenshtein_2.0.6.tgz";
        url  = "https://registry.npmjs.org/fast-levenshtein/-/fast-levenshtein-2.0.6.tgz";
        sha1 = "PYpcZog6FqMMqGQ+hR8Zuqd5eRc=";
      };
    }
    {
      name = "https___registry.npmjs.org_fastq___fastq_1.13.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_fastq___fastq_1.13.0.tgz";
        url  = "https://registry.npmjs.org/fastq/-/fastq-1.13.0.tgz";
        sha512 = "YpkpUnK8od0o1hmeSc7UUs/eB/vIPWJYjKck2QKIzAf71Vm1AAQ3EbuZB3g2JIy+pg+ERD0vqI79KyZiB2e2Nw==";
      };
    }
    {
      name = "https___registry.npmjs.org_fb_watchman___fb_watchman_2.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_fb_watchman___fb_watchman_2.0.1.tgz";
        url  = "https://registry.npmjs.org/fb-watchman/-/fb-watchman-2.0.1.tgz";
        sha512 = "DkPJKQeY6kKwmuMretBhr7G6Vodr7bFwDYTXIkfG1gjvNpaxBTQV3PbXg6bR1c1UP4jPOX0jHUbbHANL9vRjVg==";
      };
    }
    {
      name = "https___registry.npmjs.org_fill_range___fill_range_7.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_fill_range___fill_range_7.0.1.tgz";
        url  = "https://registry.npmjs.org/fill-range/-/fill-range-7.0.1.tgz";
        sha512 = "qOo9F+dMUmC2Lcb4BbVvnKJxTPjCm+RRpe4gDuGrzkL7mEVl/djYSu2OdQ2Pa302N4oqkSg9ir6jaLWJ2USVpQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_find_file_up___find_file_up_0.1.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_find_file_up___find_file_up_0.1.3.tgz";
        url  = "https://registry.npmjs.org/find-file-up/-/find-file-up-0.1.3.tgz";
        sha1 = "z2gJG8+fMApA2kEbN9pczlovvqA=";
      };
    }
    {
      name = "https___registry.npmjs.org_find_pkg___find_pkg_0.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_find_pkg___find_pkg_0.1.2.tgz";
        url  = "https://registry.npmjs.org/find-pkg/-/find-pkg-0.1.2.tgz";
        sha1 = "G9wiwG42NlUy4qJIBGhUuXiNpVc=";
      };
    }
    {
      name = "https___registry.npmjs.org_find_up___find_up_4.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_find_up___find_up_4.1.0.tgz";
        url  = "https://registry.npmjs.org/find-up/-/find-up-4.1.0.tgz";
        sha512 = "PpOwAdQ/YlXQ2vj8a3h8IipDuYRi3wceVQQGYWxNINccq40Anw7BlsEXCMbt1Zt+OLA6Fq9suIpIWD0OsnISlw==";
      };
    }
    {
      name = "https___registry.npmjs.org_form_data___form_data_3.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_form_data___form_data_3.0.1.tgz";
        url  = "https://registry.npmjs.org/form-data/-/form-data-3.0.1.tgz";
        sha512 = "RHkBKtLWUVwd7SqRIvCZMEvAMoGUp0XU+seQiZejj0COz3RI3hWP4sCv3gZWWLjJTd7rGwcsF5eKZGii0r/hbg==";
      };
    }
    {
      name = "https___registry.npmjs.org_fraction.js___fraction.js_4.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_fraction.js___fraction.js_4.2.0.tgz";
        url  = "https://registry.npmjs.org/fraction.js/-/fraction.js-4.2.0.tgz";
        sha512 = "MhLuK+2gUcnZe8ZHlaaINnQLl0xRIGRfcGk2yl8xoQAfHrSsL3rYu6FCmBdkdbhc9EPlwyGHewaRsvwRMJtAlA==";
      };
    }
    {
      name = "https___registry.npmjs.org_fs_exists_sync___fs_exists_sync_0.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_fs_exists_sync___fs_exists_sync_0.1.0.tgz";
        url  = "https://registry.npmjs.org/fs-exists-sync/-/fs-exists-sync-0.1.0.tgz";
        sha1 = "mC1ok6+RjnLQjeyehnP/K1qNat0=";
      };
    }
    {
      name = "https___registry.npmjs.org_fs.realpath___fs.realpath_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_fs.realpath___fs.realpath_1.0.0.tgz";
        url  = "https://registry.npmjs.org/fs.realpath/-/fs.realpath-1.0.0.tgz";
        sha1 = "FQStJSMVjKpA20onh8sBQRmU6k8=";
      };
    }
    {
      name = "fsevents___fsevents_2.3.2.tgz";
      path = fetchurl {
        name = "fsevents___fsevents_2.3.2.tgz";
        url  = "https://registry.yarnpkg.com/fsevents/-/fsevents-2.3.2.tgz";
        sha512 = "xiqMQR4xAeHTuB9uWm+fFRcIOgKBMiOBP+eXiyT7jsgVCq1bkVygt00oASowB7EdtpOHaaPgKt812P9ab+DDKA==";
      };
    }
    {
      name = "https___registry.npmjs.org_function_bind___function_bind_1.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_function_bind___function_bind_1.1.1.tgz";
        url  = "https://registry.npmjs.org/function-bind/-/function-bind-1.1.1.tgz";
        sha512 = "yIovAzMX49sF8Yl58fSCWJ5svSLuaibPxXQJFLmBObTuCr0Mf1KiPopGM9NiFjiYBCbfaa2Fh6breQ6ANVTI0A==";
      };
    }
    {
      name = "https___registry.npmjs.org_generic_names___generic_names_4.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_generic_names___generic_names_4.0.0.tgz";
        url  = "https://registry.npmjs.org/generic-names/-/generic-names-4.0.0.tgz";
        sha512 = "ySFolZQfw9FoDb3ed9d80Cm9f0+r7qj+HJkWjeD9RBfpxEVTlVhol+gvaQB/78WbwYfbnNh8nWHHBSlg072y6A==";
      };
    }
    {
      name = "https___registry.npmjs.org_gensync___gensync_1.0.0_beta.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_gensync___gensync_1.0.0_beta.2.tgz";
        url  = "https://registry.npmjs.org/gensync/-/gensync-1.0.0-beta.2.tgz";
        sha512 = "3hN7NaskYvMDLQY55gnW3NQ+mesEAepTqlg+VEbj7zzqEMBVNhzcGYYeqFo/TlYz6eQiFcp1HcsCZO+nGgS8zg==";
      };
    }
    {
      name = "https___registry.npmjs.org_get_caller_file___get_caller_file_2.0.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_get_caller_file___get_caller_file_2.0.5.tgz";
        url  = "https://registry.npmjs.org/get-caller-file/-/get-caller-file-2.0.5.tgz";
        sha512 = "DyFP3BM/3YHTQOCUL/w0OZHR0lpKeGrxotcHWcqNEdnltqFwXVfhEBQ94eIo34AfQpo0rGki4cyIiftY06h2Fg==";
      };
    }
    {
      name = "https___registry.npmjs.org_get_package_type___get_package_type_0.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_get_package_type___get_package_type_0.1.0.tgz";
        url  = "https://registry.npmjs.org/get-package-type/-/get-package-type-0.1.0.tgz";
        sha512 = "pjzuKtY64GYfWizNAJ0fr9VqttZkNiK2iS430LtIHzjBEr6bX8Am2zm4sW4Ro5wjWW5cAlRL1qAMTcXbjNAO2Q==";
      };
    }
    {
      name = "https___registry.npmjs.org_get_stream___get_stream_6.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_get_stream___get_stream_6.0.1.tgz";
        url  = "https://registry.npmjs.org/get-stream/-/get-stream-6.0.1.tgz";
        sha512 = "ts6Wi+2j3jQjqi70w5AlN8DFnkSwC+MqmxEzdEALB2qXZYV3X/b1CTfgPLGJNMeAWxdPfU8FO1ms3NUfaHCPYg==";
      };
    }
    {
      name = "https___registry.npmjs.org_glob_parent___glob_parent_5.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_glob_parent___glob_parent_5.1.2.tgz";
        url  = "https://registry.npmjs.org/glob-parent/-/glob-parent-5.1.2.tgz";
        sha512 = "AOIgSQCepiJYwP3ARnGx+5VnTu2HBYdzbGP45eLw1vr3zB3vZLeyed1sC9hnbcOc9/SrMyM5RPQrkGz4aS9Zow==";
      };
    }
    {
      name = "https___registry.npmjs.org_glob_parent___glob_parent_6.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_glob_parent___glob_parent_6.0.2.tgz";
        url  = "https://registry.npmjs.org/glob-parent/-/glob-parent-6.0.2.tgz";
        sha512 = "XxwI8EOhVQgWp6iDL+3b0r86f4d6AX6zSU55HfB4ydCEuXLXc5FcYeOu+nnGftS4TEju/11rt4KJPTMgbfmv4A==";
      };
    }
    {
      name = "https___registry.npmjs.org_glob___glob_7.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_glob___glob_7.2.0.tgz";
        url  = "https://registry.npmjs.org/glob/-/glob-7.2.0.tgz";
        sha512 = "lmLf6gtyrPq8tTjSmrO94wBeQbFR3HbLHbuyD69wuyQkImp2hWqMGB47OX65FBkPffO641IP9jWa1z4ivqG26Q==";
      };
    }
    {
      name = "https___registry.npmjs.org_global_modules___global_modules_0.2.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_global_modules___global_modules_0.2.3.tgz";
        url  = "https://registry.npmjs.org/global-modules/-/global-modules-0.2.3.tgz";
        sha1 = "6lo77ULG1s6ZWk+KEmm12uIjgo0=";
      };
    }
    {
      name = "https___registry.npmjs.org_global_prefix___global_prefix_0.1.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_global_prefix___global_prefix_0.1.5.tgz";
        url  = "https://registry.npmjs.org/global-prefix/-/global-prefix-0.1.5.tgz";
        sha1 = "jTvGuNo8qBEqFg2NSW/wRiv+948=";
      };
    }
    {
      name = "https___registry.npmjs.org_globals___globals_11.12.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_globals___globals_11.12.0.tgz";
        url  = "https://registry.npmjs.org/globals/-/globals-11.12.0.tgz";
        sha512 = "WOBp/EEGUiIsJSp7wcv/y6MO+lV9UoncWqxuFfm8eBwzWNgyfBd6Gz+IeKQ9jCmyhoH99g15M3T+QaVHFjizVA==";
      };
    }
    {
      name = "https___registry.npmjs.org_graceful_fs___graceful_fs_4.2.9.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_graceful_fs___graceful_fs_4.2.9.tgz";
        url  = "https://registry.npmjs.org/graceful-fs/-/graceful-fs-4.2.9.tgz";
        sha512 = "NtNxqUcXgpW2iMrfqSfR73Glt39K+BLwWsPs94yR63v45T0Wbej7eRmL5cWfwEgqXnmjQp3zaJTshdRW/qC2ZQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_has_flag___has_flag_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_has_flag___has_flag_3.0.0.tgz";
        url  = "https://registry.npmjs.org/has-flag/-/has-flag-3.0.0.tgz";
        sha1 = "tdRU3CGZriJWmfNGfloH87lVuv0=";
      };
    }
    {
      name = "https___registry.npmjs.org_has_flag___has_flag_4.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_has_flag___has_flag_4.0.0.tgz";
        url  = "https://registry.npmjs.org/has-flag/-/has-flag-4.0.0.tgz";
        sha512 = "EykJT/Q1KjTWctppgIAgfSO0tKVuZUjhgMr17kqTumMl6Afv3EISleU7qZUzoXDFTAHTDC4NOoG/ZxU3EvlMPQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_has___has_1.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_has___has_1.0.3.tgz";
        url  = "https://registry.npmjs.org/has/-/has-1.0.3.tgz";
        sha512 = "f2dvO0VU6Oej7RkWJGrehjbzMAjFp5/VKPp5tTpWIV4JHHZK1/BxbFRtf/siA2SWTe09caDmVtYYzWEIbBS4zw==";
      };
    }
    {
      name = "https___registry.npmjs.org_homedir_polyfill___homedir_polyfill_1.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_homedir_polyfill___homedir_polyfill_1.0.3.tgz";
        url  = "https://registry.npmjs.org/homedir-polyfill/-/homedir-polyfill-1.0.3.tgz";
        sha512 = "eSmmWE5bZTK2Nou4g0AI3zZ9rswp7GRKoKXS1BLUkvPviOqs4YTN1djQIqrXy9k5gEtdLPy86JjRwsNM9tnDcA==";
      };
    }
    {
      name = "https___registry.npmjs.org_html_encoding_sniffer___html_encoding_sniffer_2.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_html_encoding_sniffer___html_encoding_sniffer_2.0.1.tgz";
        url  = "https://registry.npmjs.org/html-encoding-sniffer/-/html-encoding-sniffer-2.0.1.tgz";
        sha512 = "D5JbOMBIR/TVZkubHT+OyT2705QvogUW4IBn6nHd756OwieSF9aDYFj4dv6HHEVGYbHaLETa3WggZYWWMyy3ZQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_html_escaper___html_escaper_2.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_html_escaper___html_escaper_2.0.2.tgz";
        url  = "https://registry.npmjs.org/html-escaper/-/html-escaper-2.0.2.tgz";
        sha512 = "H2iMtd0I4Mt5eYiapRdIDjp+XzelXQ0tFE4JS7YFwFevXXMmOp9myNrUvCg0D6ws8iqkRPBfKHgbwig1SmlLfg==";
      };
    }
    {
      name = "https___registry.npmjs.org_http_proxy_agent___http_proxy_agent_4.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_http_proxy_agent___http_proxy_agent_4.0.1.tgz";
        url  = "https://registry.npmjs.org/http-proxy-agent/-/http-proxy-agent-4.0.1.tgz";
        sha512 = "k0zdNgqWTGA6aeIRVpvfVob4fL52dTfaehylg0Y4UvSySvOq/Y+BOyPrgpUrA7HylqvU8vIZGsRuXmspskV0Tg==";
      };
    }
    {
      name = "https___registry.npmjs.org_https_proxy_agent___https_proxy_agent_5.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_https_proxy_agent___https_proxy_agent_5.0.0.tgz";
        url  = "https://registry.npmjs.org/https-proxy-agent/-/https-proxy-agent-5.0.0.tgz";
        sha512 = "EkYm5BcKUGiduxzSt3Eppko+PiNWNEpa4ySk9vTC6wDsQJW9rHSa+UhGNJoRYp7bz6Ht1eaRIa6QaJqO5rCFbA==";
      };
    }
    {
      name = "https___registry.npmjs.org_human_signals___human_signals_2.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_human_signals___human_signals_2.1.0.tgz";
        url  = "https://registry.npmjs.org/human-signals/-/human-signals-2.1.0.tgz";
        sha512 = "B4FFZ6q/T2jhhksgkbEW3HBvWIfDW85snkQgawt07S7J5QXTk6BkNV+0yAeZrM5QpMAdYlocGoljn0sJ/WQkFw==";
      };
    }
    {
      name = "https___registry.npmjs.org_iconv_lite___iconv_lite_0.4.24.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_iconv_lite___iconv_lite_0.4.24.tgz";
        url  = "https://registry.npmjs.org/iconv-lite/-/iconv-lite-0.4.24.tgz";
        sha512 = "v3MXnZAcvnywkTUEZomIActle7RXXeedOR31wwl7VlyoXO4Qi9arvSenNQWne1TcRwhCL1HwLI21bEqdpj8/rA==";
      };
    }
    {
      name = "https___registry.npmjs.org_icss_replace_symbols___icss_replace_symbols_1.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_icss_replace_symbols___icss_replace_symbols_1.1.0.tgz";
        url  = "https://registry.npmjs.org/icss-replace-symbols/-/icss-replace-symbols-1.1.0.tgz";
        sha1 = "Bupvg2ead0njhs/h/oEq5dsiPe0=";
      };
    }
    {
      name = "https___registry.npmjs.org_icss_utils___icss_utils_5.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_icss_utils___icss_utils_5.1.0.tgz";
        url  = "https://registry.npmjs.org/icss-utils/-/icss-utils-5.1.0.tgz";
        sha512 = "soFhflCVWLfRNOPU3iv5Z9VUdT44xFRbzjLsEzSr5AQmgqPMTHdU3PMT1Cf1ssx8fLNJDA1juftYl+PUcv3MqA==";
      };
    }
    {
      name = "https___registry.npmjs.org_idb___idb_7.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_idb___idb_7.0.1.tgz";
        url  = "https://registry.npmjs.org/idb/-/idb-7.0.1.tgz";
        sha512 = "UUxlE7vGWK5RfB/fDwEGgRf84DY/ieqNha6msMV99UsEMQhJ1RwbCd8AYBj3QMgnE3VZnfQvm4oKVCJTYlqIgg==";
      };
    }
    {
      name = "https___registry.npmjs.org_import_fresh___import_fresh_3.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_import_fresh___import_fresh_3.3.0.tgz";
        url  = "https://registry.npmjs.org/import-fresh/-/import-fresh-3.3.0.tgz";
        sha512 = "veYYhQa+D1QBKznvhUHxb8faxlrwUnxseDAbAp457E0wLNio2bOSKnjYDhMj+YiAq61xrMGhQk9iXVk5FzgQMw==";
      };
    }
    {
      name = "https___registry.npmjs.org_import_local___import_local_3.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_import_local___import_local_3.1.0.tgz";
        url  = "https://registry.npmjs.org/import-local/-/import-local-3.1.0.tgz";
        sha512 = "ASB07uLtnDs1o6EHjKpX34BKYDSqnFerfTOJL2HvMqF70LnxpjkzDB8J44oT9pu4AMPkQwf8jl6szgvNd2tRIg==";
      };
    }
    {
      name = "https___registry.npmjs.org_imurmurhash___imurmurhash_0.1.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_imurmurhash___imurmurhash_0.1.4.tgz";
        url  = "https://registry.npmjs.org/imurmurhash/-/imurmurhash-0.1.4.tgz";
        sha1 = "khi5srkoojixPcT7a21XbyMUU+o=";
      };
    }
    {
      name = "https___registry.npmjs.org_incremental_dom___incremental_dom_0.7.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_incremental_dom___incremental_dom_0.7.0.tgz";
        url  = "https://registry.npmjs.org/incremental-dom/-/incremental-dom-0.7.0.tgz";
        sha512 = "SBHQ6AiCmtwh7TU9hjq2CspasJe7ggGa9k+qYZft+d5Qq9v7V+07wlnRSZH5GGYjI8wn6U5p7dDua7f1bih52g==";
      };
    }
    {
      name = "https___registry.npmjs.org_inflight___inflight_1.0.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_inflight___inflight_1.0.6.tgz";
        url  = "https://registry.npmjs.org/inflight/-/inflight-1.0.6.tgz";
        sha1 = "Sb1jMdfQLQwJvJEKEHW6gWW1bfk=";
      };
    }
    {
      name = "https___registry.npmjs.org_inherits___inherits_2.0.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_inherits___inherits_2.0.4.tgz";
        url  = "https://registry.npmjs.org/inherits/-/inherits-2.0.4.tgz";
        sha512 = "k/vGaX4/Yla3WzyMCvTQOXYeIHvqOKtnqBduzTHpzpQZzAskKMhZ2K+EnBiSM9zGSoIFeMpXKxa4dYeZIQqewQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_ini___ini_1.3.8.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ini___ini_1.3.8.tgz";
        url  = "https://registry.npmjs.org/ini/-/ini-1.3.8.tgz";
        sha512 = "JV/yugV2uzW5iMRSiZAyDtQd+nxtUnjeLt0acNdw98kKLrvuRVyB80tsREOE7yvGVgalhZ6RNXCmEHkUKBKxew==";
      };
    }
    {
      name = "https___registry.npmjs.org_is_arrayish___is_arrayish_0.2.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_arrayish___is_arrayish_0.2.1.tgz";
        url  = "https://registry.npmjs.org/is-arrayish/-/is-arrayish-0.2.1.tgz";
        sha1 = "d8mYQFJ6qOyxqLppe4BkWnqSap0=";
      };
    }
    {
      name = "https___registry.npmjs.org_is_binary_path___is_binary_path_2.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_binary_path___is_binary_path_2.1.0.tgz";
        url  = "https://registry.npmjs.org/is-binary-path/-/is-binary-path-2.1.0.tgz";
        sha512 = "ZMERYes6pDydyuGidse7OsHxtbI7WVeUEozgR/g7rd0xUimYNlvZRE/K2MgZTjWy725IfelLeVcEM97mmtRGXw==";
      };
    }
    {
      name = "https___registry.npmjs.org_is_buffer___is_buffer_1.1.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_buffer___is_buffer_1.1.6.tgz";
        url  = "https://registry.npmjs.org/is-buffer/-/is-buffer-1.1.6.tgz";
        sha512 = "NcdALwpXkTm5Zvvbk7owOUSvVvBKDgKP5/ewfXEznmQFfs4ZRmanOeKBTjRVjka3QFoN6XJ+9F3USqfHqTaU5w==";
      };
    }
    {
      name = "https___registry.npmjs.org_is_core_module___is_core_module_2.8.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_core_module___is_core_module_2.8.1.tgz";
        url  = "https://registry.npmjs.org/is-core-module/-/is-core-module-2.8.1.tgz";
        sha512 = "SdNCUs284hr40hFTFP6l0IfZ/RSrMXF3qgoRHd3/79unUTvrFO/JoXwkGm+5J/Oe3E/b5GsnG330uUNgRpu1PA==";
      };
    }
    {
      name = "https___registry.npmjs.org_is_extendable___is_extendable_0.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_extendable___is_extendable_0.1.1.tgz";
        url  = "https://registry.npmjs.org/is-extendable/-/is-extendable-0.1.1.tgz";
        sha1 = "YrEQ4omkcUGOPsNqYX1HLjAd/Ik=";
      };
    }
    {
      name = "https___registry.npmjs.org_is_extglob___is_extglob_2.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_extglob___is_extglob_2.1.1.tgz";
        url  = "https://registry.npmjs.org/is-extglob/-/is-extglob-2.1.1.tgz";
        sha1 = "qIwCU1eR8C7TfHahueqXc8gz+MI=";
      };
    }
    {
      name = "https___registry.npmjs.org_is_fullwidth_code_point___is_fullwidth_code_point_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_fullwidth_code_point___is_fullwidth_code_point_3.0.0.tgz";
        url  = "https://registry.npmjs.org/is-fullwidth-code-point/-/is-fullwidth-code-point-3.0.0.tgz";
        sha512 = "zymm5+u+sCsSWyD9qNaejV3DFvhCKclKdizYaJUuHA83RLjb7nSuGnddCHGv0hk+KY7BMAlsWeK4Ueg6EV6XQg==";
      };
    }
    {
      name = "https___registry.npmjs.org_is_generator_fn___is_generator_fn_2.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_generator_fn___is_generator_fn_2.1.0.tgz";
        url  = "https://registry.npmjs.org/is-generator-fn/-/is-generator-fn-2.1.0.tgz";
        sha512 = "cTIB4yPYL/Grw0EaSzASzg6bBy9gqCofvWN8okThAYIxKJZC+udlRAmGbM0XLeniEJSs8uEgHPGuHSe1XsOLSQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_is_glob___is_glob_4.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_glob___is_glob_4.0.3.tgz";
        url  = "https://registry.npmjs.org/is-glob/-/is-glob-4.0.3.tgz";
        sha512 = "xelSayHH36ZgE7ZWhli7pW34hNbNl8Ojv5KVmkJD4hBdD3th8Tfk9vYasLM+mXWOZhFkgZfxhLSnrwRr4elSSg==";
      };
    }
    {
      name = "https___registry.npmjs.org_is_number___is_number_7.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_number___is_number_7.0.0.tgz";
        url  = "https://registry.npmjs.org/is-number/-/is-number-7.0.0.tgz";
        sha512 = "41Cifkg6e8TylSpdtTpeLVMqvSBEVzTttHvERD741+pnZ8ANv0004MRL43QKPDlK9cGvNp6NZWZUBlbGXYxxng==";
      };
    }
    {
      name = "https___registry.npmjs.org_is_potential_custom_element_name___is_potential_custom_element_name_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_potential_custom_element_name___is_potential_custom_element_name_1.0.1.tgz";
        url  = "https://registry.npmjs.org/is-potential-custom-element-name/-/is-potential-custom-element-name-1.0.1.tgz";
        sha512 = "bCYeRA2rVibKZd+s2625gGnGF/t7DSqDs4dP7CrLA1m7jKWz6pps0LpYLJN8Q64HtmPKJ1hrN3nzPNKFEKOUiQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_is_stream___is_stream_2.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_stream___is_stream_2.0.1.tgz";
        url  = "https://registry.npmjs.org/is-stream/-/is-stream-2.0.1.tgz";
        sha512 = "hFoiJiTl63nn+kstHGBtewWSKnQLpyb155KHheA1l39uvtO9nWIop1p3udqPcUd/xbF1VLMO4n7OI6p7RbngDg==";
      };
    }
    {
      name = "https___registry.npmjs.org_is_typedarray___is_typedarray_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_typedarray___is_typedarray_1.0.0.tgz";
        url  = "https://registry.npmjs.org/is-typedarray/-/is-typedarray-1.0.0.tgz";
        sha1 = "5HnICFjfDBsR3dppQPlgEfzaSpo=";
      };
    }
    {
      name = "https___registry.npmjs.org_is_windows___is_windows_0.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_is_windows___is_windows_0.2.0.tgz";
        url  = "https://registry.npmjs.org/is-windows/-/is-windows-0.2.0.tgz";
        sha1 = "3hqm1j6indJIc3tp8f+LgALSEIw=";
      };
    }
    {
      name = "https___registry.npmjs.org_isexe___isexe_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_isexe___isexe_2.0.0.tgz";
        url  = "https://registry.npmjs.org/isexe/-/isexe-2.0.0.tgz";
        sha1 = "6PvzdNxVb/iUehDcsFctYz8s+hA=";
      };
    }
    {
      name = "https___registry.npmjs.org_istanbul_lib_coverage___istanbul_lib_coverage_3.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_istanbul_lib_coverage___istanbul_lib_coverage_3.2.0.tgz";
        url  = "https://registry.npmjs.org/istanbul-lib-coverage/-/istanbul-lib-coverage-3.2.0.tgz";
        sha512 = "eOeJ5BHCmHYvQK7xt9GkdHuzuCGS1Y6g9Gvnx3Ym33fz/HpLRYxiS0wHNr+m/MBC8B647Xt608vCDEvhl9c6Mw==";
      };
    }
    {
      name = "https___registry.npmjs.org_istanbul_lib_instrument___istanbul_lib_instrument_5.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_istanbul_lib_instrument___istanbul_lib_instrument_5.1.0.tgz";
        url  = "https://registry.npmjs.org/istanbul-lib-instrument/-/istanbul-lib-instrument-5.1.0.tgz";
        sha512 = "czwUz525rkOFDJxfKK6mYfIs9zBKILyrZQxjz3ABhjQXhbhFsSbo1HW/BFcsDnfJYJWA6thRR5/TUY2qs5W99Q==";
      };
    }
    {
      name = "https___registry.npmjs.org_istanbul_lib_report___istanbul_lib_report_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_istanbul_lib_report___istanbul_lib_report_3.0.0.tgz";
        url  = "https://registry.npmjs.org/istanbul-lib-report/-/istanbul-lib-report-3.0.0.tgz";
        sha512 = "wcdi+uAKzfiGT2abPpKZ0hSU1rGQjUQnLvtY5MpQ7QCTahD3VODhcu4wcfY1YtkGaDD5yuydOLINXsfbus9ROw==";
      };
    }
    {
      name = "https___registry.npmjs.org_istanbul_lib_source_maps___istanbul_lib_source_maps_4.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_istanbul_lib_source_maps___istanbul_lib_source_maps_4.0.1.tgz";
        url  = "https://registry.npmjs.org/istanbul-lib-source-maps/-/istanbul-lib-source-maps-4.0.1.tgz";
        sha512 = "n3s8EwkdFIJCG3BPKBYvskgXGoy88ARzvegkitk60NxRdwltLOTaH7CUiMRXvwYorl0Q712iEjcWB+fK/MrWVw==";
      };
    }
    {
      name = "https___registry.npmjs.org_istanbul_reports___istanbul_reports_3.1.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_istanbul_reports___istanbul_reports_3.1.4.tgz";
        url  = "https://registry.npmjs.org/istanbul-reports/-/istanbul-reports-3.1.4.tgz";
        sha512 = "r1/DshN4KSE7xWEknZLLLLDn5CJybV3nw01VTkp6D5jzLuELlcbudfj/eSQFvrKsJuTVCGnePO7ho82Nw9zzfw==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_changed_files___jest_changed_files_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_changed_files___jest_changed_files_27.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-changed-files/-/jest-changed-files-27.5.1.tgz";
        sha512 = "buBLMiByfWGCoMsLLzGUUSpAmIAGnbR2KJoMN10ziLhOLvP4e0SlypHnAel8iqQXTrcbmfEY9sSqae5sgUsTvw==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_circus___jest_circus_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_circus___jest_circus_27.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-circus/-/jest-circus-27.5.1.tgz";
        sha512 = "D95R7x5UtlMA5iBYsOHFFbMD/GVA4R/Kdq15f7xYWUfWHBto9NYRsOvnSauTgdF+ogCpJ4tyKOXhUifxS65gdw==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_cli___jest_cli_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_cli___jest_cli_27.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-cli/-/jest-cli-27.5.1.tgz";
        sha512 = "Hc6HOOwYq4/74/c62dEE3r5elx8wjYqxY0r0G/nFrLDPMFRu6RA/u8qINOIkvhxG7mMQ5EJsOGfRpI8L6eFUVw==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_config___jest_config_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_config___jest_config_27.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-config/-/jest-config-27.5.1.tgz";
        sha512 = "5sAsjm6tGdsVbW9ahcChPAFCk4IlkQUknH5AvKjuLTSlcO/wCZKyFdn7Rg0EkC+OGgWODEy2hDpWB1PgzH0JNA==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_diff___jest_diff_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_diff___jest_diff_27.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-diff/-/jest-diff-27.5.1.tgz";
        sha512 = "m0NvkX55LDt9T4mctTEgnZk3fmEg3NRYutvMPWM/0iPnkFj2wIeF45O1718cMSOFO1vINkqmxqD8vE37uTEbqw==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_docblock___jest_docblock_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_docblock___jest_docblock_27.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-docblock/-/jest-docblock-27.5.1.tgz";
        sha512 = "rl7hlABeTsRYxKiUfpHrQrG4e2obOiTQWfMEH3PxPjOtdsfLQO4ReWSZaQ7DETm4xu07rl4q/h4zcKXyU0/OzQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_each___jest_each_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_each___jest_each_27.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-each/-/jest-each-27.5.1.tgz";
        sha512 = "1Ff6p+FbhT/bXQnEouYy00bkNSY7OUpfIcmdl8vZ31A1UUaurOLPA8a8BbJOF2RDUElwJhmeaV7LnagI+5UwNQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_environment_jsdom___jest_environment_jsdom_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_environment_jsdom___jest_environment_jsdom_27.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-environment-jsdom/-/jest-environment-jsdom-27.5.1.tgz";
        sha512 = "TFBvkTC1Hnnnrka/fUb56atfDtJ9VMZ94JkjTbggl1PEpwrYtUBKMezB3inLmWqQsXYLcMwNoDQwoBTAvFfsfw==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_environment_node___jest_environment_node_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_environment_node___jest_environment_node_27.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-environment-node/-/jest-environment-node-27.5.1.tgz";
        sha512 = "Jt4ZUnxdOsTGwSRAfKEnE6BcwsSPNOijjwifq5sDFSA2kesnXTvNqKHYgM0hDq3549Uf/KzdXNYn4wMZJPlFLw==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_get_type___jest_get_type_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_get_type___jest_get_type_27.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-get-type/-/jest-get-type-27.5.1.tgz";
        sha512 = "2KY95ksYSaK7DMBWQn6dQz3kqAf3BB64y2udeG+hv4KfSOb9qwcYQstTJc1KCbsix+wLZWZYN8t7nwX3GOBLRw==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_haste_map___jest_haste_map_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_haste_map___jest_haste_map_27.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-haste-map/-/jest-haste-map-27.5.1.tgz";
        sha512 = "7GgkZ4Fw4NFbMSDSpZwXeBiIbx+t/46nJ2QitkOjvwPYyZmqttu2TDSimMHP1EkPOi4xUZAN1doE5Vd25H4Jng==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_jasmine2___jest_jasmine2_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_jasmine2___jest_jasmine2_27.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-jasmine2/-/jest-jasmine2-27.5.1.tgz";
        sha512 = "jtq7VVyG8SqAorDpApwiJJImd0V2wv1xzdheGHRGyuT7gZm6gG47QEskOlzsN1PG/6WNaCo5pmwMHDf3AkG2pQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_leak_detector___jest_leak_detector_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_leak_detector___jest_leak_detector_27.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-leak-detector/-/jest-leak-detector-27.5.1.tgz";
        sha512 = "POXfWAMvfU6WMUXftV4HolnJfnPOGEu10fscNCA76KBpRRhcMN2c8d3iT2pxQS3HLbA+5X4sOUPzYO2NUyIlHQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_matcher_utils___jest_matcher_utils_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_matcher_utils___jest_matcher_utils_27.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-matcher-utils/-/jest-matcher-utils-27.5.1.tgz";
        sha512 = "z2uTx/T6LBaCoNWNFWwChLBKYxTMcGBRjAt+2SbP929/Fflb9aa5LGma654Rz8z9HLxsrUaYzxE9T/EFIL/PAw==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_message_util___jest_message_util_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_message_util___jest_message_util_27.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-message-util/-/jest-message-util-27.5.1.tgz";
        sha512 = "rMyFe1+jnyAAf+NHwTclDz0eAaLkVDdKVHHBFWsBWHnnh5YeJMNWWsv7AbFYXfK3oTqvL7VTWkhNLu1jX24D+g==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_mock___jest_mock_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_mock___jest_mock_27.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-mock/-/jest-mock-27.5.1.tgz";
        sha512 = "K4jKbY1d4ENhbrG2zuPWaQBvDly+iZ2yAW+T1fATN78hc0sInwn7wZB8XtlNnvHug5RMwV897Xm4LqmPM4e2Og==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_pnp_resolver___jest_pnp_resolver_1.2.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_pnp_resolver___jest_pnp_resolver_1.2.2.tgz";
        url  = "https://registry.npmjs.org/jest-pnp-resolver/-/jest-pnp-resolver-1.2.2.tgz";
        sha512 = "olV41bKSMm8BdnuMsewT4jqlZ8+3TCARAXjZGT9jcoSnrfUnRCqnMoF9XEeoWjbzObpqF9dRhHQj0Xb9QdF6/w==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_regex_util___jest_regex_util_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_regex_util___jest_regex_util_27.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-regex-util/-/jest-regex-util-27.5.1.tgz";
        sha512 = "4bfKq2zie+x16okqDXjXn9ql2B0dScQu+vcwe4TvFVhkVyuWLqpZrZtXxLLWoXYgn0E87I6r6GRYHF7wFZBUvg==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_resolve_dependencies___jest_resolve_dependencies_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_resolve_dependencies___jest_resolve_dependencies_27.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-resolve-dependencies/-/jest-resolve-dependencies-27.5.1.tgz";
        sha512 = "QQOOdY4PE39iawDn5rzbIePNigfe5B9Z91GDD1ae/xNDlu9kaat8QQ5EKnNmVWPV54hUdxCVwwj6YMgR2O7IOg==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_resolve___jest_resolve_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_resolve___jest_resolve_27.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-resolve/-/jest-resolve-27.5.1.tgz";
        sha512 = "FFDy8/9E6CV83IMbDpcjOhumAQPDyETnU2KZ1O98DwTnz8AOBsW/Xv3GySr1mOZdItLR+zDZ7I/UdTFbgSOVCw==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_runner___jest_runner_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_runner___jest_runner_27.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-runner/-/jest-runner-27.5.1.tgz";
        sha512 = "g4NPsM4mFCOwFKXO4p/H/kWGdJp9V8kURY2lX8Me2drgXqG7rrZAx5kv+5H7wtt/cdFIjhqYx1HrlqWHaOvDaQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_runtime___jest_runtime_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_runtime___jest_runtime_27.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-runtime/-/jest-runtime-27.5.1.tgz";
        sha512 = "o7gxw3Gf+H2IGt8fv0RiyE1+r83FJBRruoA+FXrlHw6xEyBsU8ugA6IPfTdVyA0w8HClpbK+DGJxH59UrNMx8A==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_serializer___jest_serializer_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_serializer___jest_serializer_27.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-serializer/-/jest-serializer-27.5.1.tgz";
        sha512 = "jZCyo6iIxO1aqUxpuBlwTDMkzOAJS4a3eYz3YzgxxVQFwLeSA7Jfq5cbqCY+JLvTDrWirgusI/0KwxKMgrdf7w==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_snapshot___jest_snapshot_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_snapshot___jest_snapshot_27.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-snapshot/-/jest-snapshot-27.5.1.tgz";
        sha512 = "yYykXI5a0I31xX67mgeLw1DZ0bJB+gpq5IpSuCAoyDi0+BhgU/RIrL+RTzDmkNTchvDFWKP8lp+w/42Z3us5sA==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_util___jest_util_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_util___jest_util_27.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-util/-/jest-util-27.5.1.tgz";
        sha512 = "Kv2o/8jNvX1MQ0KGtw480E/w4fBCDOnH6+6DmeKi6LZUIlKA5kwY0YNdlzaWTiVgxqAqik11QyxDOKk543aKXw==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_validate___jest_validate_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_validate___jest_validate_27.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-validate/-/jest-validate-27.5.1.tgz";
        sha512 = "thkNli0LYTmOI1tDB3FI1S1RTp/Bqyd9pTarJwL87OIBFuqEb5Apv5EaApEudYg4g86e3CT6kM0RowkhtEnCBQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_watcher___jest_watcher_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_watcher___jest_watcher_27.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-watcher/-/jest-watcher-27.5.1.tgz";
        sha512 = "z676SuD6Z8o8qbmEGhoEUFOM1+jfEiL3DXHK/xgEiG2EyNYfFG60jluWcupY6dATjfEsKQuibReS1djInQnoVw==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest_worker___jest_worker_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest_worker___jest_worker_27.5.1.tgz";
        url  = "https://registry.npmjs.org/jest-worker/-/jest-worker-27.5.1.tgz";
        sha512 = "7vuh85V5cdDofPyxn58nrPjBktZo0u9x1g8WtjQol+jZDaE+fhN+cIvTj11GndBnMnyfrUOG1sZQxCdjKh+DKg==";
      };
    }
    {
      name = "https___registry.npmjs.org_jest___jest_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jest___jest_27.5.1.tgz";
        url  = "https://registry.npmjs.org/jest/-/jest-27.5.1.tgz";
        sha512 = "Yn0mADZB89zTtjkPJEXwrac3LHudkQMR+Paqa8uxJHCBr9agxztUifWCyiYrjhMPBoUVBjyny0I7XH6ozDr7QQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_js_tokens___js_tokens_4.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_js_tokens___js_tokens_4.0.0.tgz";
        url  = "https://registry.npmjs.org/js-tokens/-/js-tokens-4.0.0.tgz";
        sha512 = "RdJUflcE3cUzKiMqQgsCu06FPu9UdIJO0beYbPhHN4k6apgJtifcoCtT9bcxOpYBtpD2kCM6Sbzg4CausW/PKQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_js_yaml___js_yaml_3.14.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_js_yaml___js_yaml_3.14.1.tgz";
        url  = "https://registry.npmjs.org/js-yaml/-/js-yaml-3.14.1.tgz";
        sha512 = "okMH7OXXJ7YrN9Ok3/SXrnu4iX9yOk+25nqX4imS2npuvTYDmo/QEZoqwZkYaIDk3jVvBOTOIEgEhaLOynBS9g==";
      };
    }
    {
      name = "https___registry.npmjs.org_jsdom___jsdom_16.7.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jsdom___jsdom_16.7.0.tgz";
        url  = "https://registry.npmjs.org/jsdom/-/jsdom-16.7.0.tgz";
        sha512 = "u9Smc2G1USStM+s/x1ru5Sxrl6mPYCbByG1U/hUmqaVsm4tbNyS7CicOSRyuGQYZhTu0h84qkZZQ/I+dzizSVw==";
      };
    }
    {
      name = "https___registry.npmjs.org_jsesc___jsesc_2.5.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_jsesc___jsesc_2.5.2.tgz";
        url  = "https://registry.npmjs.org/jsesc/-/jsesc-2.5.2.tgz";
        sha512 = "OYu7XEzjkCQ3C5Ps3QIZsQfNpqoJyZZA99wd9aWd05NCtC5pWOkShK2mkL6HXQR6/Cy2lbNdPlZBpuQHXE63gA==";
      };
    }
    {
      name = "https___registry.npmjs.org_json_parse_even_better_errors___json_parse_even_better_errors_2.3.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_json_parse_even_better_errors___json_parse_even_better_errors_2.3.1.tgz";
        url  = "https://registry.npmjs.org/json-parse-even-better-errors/-/json-parse-even-better-errors-2.3.1.tgz";
        sha512 = "xyFwyhro/JEof6Ghe2iz2NcXoj2sloNsWr/XsERDK/oiPCfaNhl5ONfp+jQdAZRQQ0IJWNzH9zIZF7li91kh2w==";
      };
    }
    {
      name = "https___registry.npmjs.org_json5___json5_2.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_json5___json5_2.2.0.tgz";
        url  = "https://registry.npmjs.org/json5/-/json5-2.2.0.tgz";
        sha512 = "f+8cldu7X/y7RAJurMEJmdoKXGB/X550w2Nr3tTbezL6RwEE/iMcm+tZnXeoZtKuOq6ft8+CqzEkrIgx1fPoQA==";
      };
    }
    {
      name = "https___registry.npmjs.org_kind_of___kind_of_3.2.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_kind_of___kind_of_3.2.2.tgz";
        url  = "https://registry.npmjs.org/kind-of/-/kind-of-3.2.2.tgz";
        sha1 = "MeohpzS6ubuw8yRm2JOupR5KPGQ=";
      };
    }
    {
      name = "https___registry.npmjs.org_kleur___kleur_3.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_kleur___kleur_3.0.3.tgz";
        url  = "https://registry.npmjs.org/kleur/-/kleur-3.0.3.tgz";
        sha512 = "eTIzlVOSUR+JxdDFepEYcBMtZ9Qqdef+rnzWdRZuMbOywu5tO2w2N7rqjoANZ5k9vywhL6Br1VRjUIgTQx4E8w==";
      };
    }
    {
      name = "https___registry.npmjs.org_lazy_cache___lazy_cache_2.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_lazy_cache___lazy_cache_2.0.2.tgz";
        url  = "https://registry.npmjs.org/lazy-cache/-/lazy-cache-2.0.2.tgz";
        sha1 = "uRkKT5EzVGlIQIWfio9whNiCImQ=";
      };
    }
    {
      name = "https___registry.npmjs.org_leven___leven_3.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_leven___leven_3.1.0.tgz";
        url  = "https://registry.npmjs.org/leven/-/leven-3.1.0.tgz";
        sha512 = "qsda+H8jTaUaN/x5vzW2rzc+8Rw4TAQ/4KjB46IwK5VH+IlVeeeje/EoZRpiXvIqjFgK84QffqPztGI3VBLG1A==";
      };
    }
    {
      name = "https___registry.npmjs.org_levn___levn_0.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_levn___levn_0.3.0.tgz";
        url  = "https://registry.npmjs.org/levn/-/levn-0.3.0.tgz";
        sha1 = "OwmSTt+fCDwEkP3UwLxEIeBHZO4=";
      };
    }
    {
      name = "https___registry.npmjs.org_lilconfig___lilconfig_2.0.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_lilconfig___lilconfig_2.0.4.tgz";
        url  = "https://registry.npmjs.org/lilconfig/-/lilconfig-2.0.4.tgz";
        sha512 = "bfTIN7lEsiooCocSISTWXkiWJkRqtL9wYtYy+8EK3Y41qh3mpwPU0ycTOgjdY9ErwXCc8QyrQp82bdL0Xkm9yA==";
      };
    }
    {
      name = "https___registry.npmjs.org_lines_and_columns___lines_and_columns_1.2.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_lines_and_columns___lines_and_columns_1.2.4.tgz";
        url  = "https://registry.npmjs.org/lines-and-columns/-/lines-and-columns-1.2.4.tgz";
        sha512 = "7ylylesZQ/PV29jhEDl3Ufjo6ZX7gCqJr5F7PKrqc93v7fzSymt1BpwEU8nAUXs8qzzvqhbjhK5QZg6Mt/HkBg==";
      };
    }
    {
      name = "https___registry.npmjs.org_loader_utils___loader_utils_3.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_loader_utils___loader_utils_3.2.0.tgz";
        url  = "https://registry.npmjs.org/loader-utils/-/loader-utils-3.2.0.tgz";
        sha512 = "HVl9ZqccQihZ7JM85dco1MvO9G+ONvxoGa9rkhzFsneGLKSUg1gJf9bWzhRhcvm2qChhWpebQhP44qxjKIUCaQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_locate_path___locate_path_5.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_locate_path___locate_path_5.0.0.tgz";
        url  = "https://registry.npmjs.org/locate-path/-/locate-path-5.0.0.tgz";
        sha512 = "t7hw9pI+WvuwNJXwk5zVHpyhIqzg2qTlklJOf0mVxGSbe3Fp2VieZcduNYjaLDoy6p9uGpQEGWG87WpMKlNq8g==";
      };
    }
    {
      name = "https___registry.npmjs.org_lodash.camelcase___lodash.camelcase_4.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_lodash.camelcase___lodash.camelcase_4.3.0.tgz";
        url  = "https://registry.npmjs.org/lodash.camelcase/-/lodash.camelcase-4.3.0.tgz";
        sha1 = "soqmKIorn8ZRA1x3EfZathkDMaY=";
      };
    }
    {
      name = "https___registry.npmjs.org_lodash.memoize___lodash.memoize_4.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_lodash.memoize___lodash.memoize_4.1.2.tgz";
        url  = "https://registry.npmjs.org/lodash.memoize/-/lodash.memoize-4.1.2.tgz";
        sha1 = "vMbEmkKihA7Zl/Mj6tpezRguC/4=";
      };
    }
    {
      name = "https___registry.npmjs.org_lodash___lodash_4.17.21.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_lodash___lodash_4.17.21.tgz";
        url  = "https://registry.npmjs.org/lodash/-/lodash-4.17.21.tgz";
        sha512 = "v2kDEe57lecTulaDIuNTPy3Ry4gLGJ6Z1O3vE1krgXZNrsQ+LFTGHVxVjcXPs17LhbZVGedAJv8XZ1tvj5FvSg==";
      };
    }
    {
      name = "https___registry.npmjs.org_lru_cache___lru_cache_6.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_lru_cache___lru_cache_6.0.0.tgz";
        url  = "https://registry.npmjs.org/lru-cache/-/lru-cache-6.0.0.tgz";
        sha512 = "Jo6dJ04CmSjuznwJSS3pUeWmd/H0ffTlkXXgwZi+eq1UCmqQwCh+eLsYOYCwY991i2Fah4h1BEMCx4qThGbsiA==";
      };
    }
    {
      name = "https___registry.npmjs.org_make_dir___make_dir_3.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_make_dir___make_dir_3.1.0.tgz";
        url  = "https://registry.npmjs.org/make-dir/-/make-dir-3.1.0.tgz";
        sha512 = "g3FeP20LNwhALb/6Cz6Dd4F2ngze0jz7tbzrD2wAV+o9FeNHe4rL+yK2md0J/fiSf1sa1ADhXqi5+oVwOM/eGw==";
      };
    }
    {
      name = "https___registry.npmjs.org_make_error___make_error_1.3.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_make_error___make_error_1.3.6.tgz";
        url  = "https://registry.npmjs.org/make-error/-/make-error-1.3.6.tgz";
        sha512 = "s8UhlNe7vPKomQhC1qFelMokr/Sc3AgNbso3n74mVPA5LTZwkB9NlXf4XPamLxJE8h0gh73rM94xvwRT2CVInw==";
      };
    }
    {
      name = "https___registry.npmjs.org_makeerror___makeerror_1.0.12.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_makeerror___makeerror_1.0.12.tgz";
        url  = "https://registry.npmjs.org/makeerror/-/makeerror-1.0.12.tgz";
        sha512 = "JmqCvUhmt43madlpFzG4BQzG2Z3m6tvQDNKdClZnO3VbIudJYmxsT0FNJMeiB2+JTSlTQTSbU8QdesVmwJcmLg==";
      };
    }
    {
      name = "https___registry.npmjs.org_merge_stream___merge_stream_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_merge_stream___merge_stream_2.0.0.tgz";
        url  = "https://registry.npmjs.org/merge-stream/-/merge-stream-2.0.0.tgz";
        sha512 = "abv/qOcuPfk3URPfDzmZU1LKmuw8kT+0nIHvKrKgFrwifol/doWcdA4ZqsWQ8ENrFKkd67Mfpo/LovbIUsbt3w==";
      };
    }
    {
      name = "https___registry.npmjs.org_merge2___merge2_1.4.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_merge2___merge2_1.4.1.tgz";
        url  = "https://registry.npmjs.org/merge2/-/merge2-1.4.1.tgz";
        sha512 = "8q7VEgMJW4J8tcfVPy8g09NcQwZdbwFEqhe/WZkoIzjn/3TGDwtOCYtXGxA3O8tPzpczCCDgv+P2P5y00ZJOOg==";
      };
    }
    {
      name = "https___registry.npmjs.org_micromatch___micromatch_4.0.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_micromatch___micromatch_4.0.4.tgz";
        url  = "https://registry.npmjs.org/micromatch/-/micromatch-4.0.4.tgz";
        sha512 = "pRmzw/XUcwXGpD9aI9q/0XOwLNygjETJ8y0ao0wdqprrzDa4YnxLcz7fQRZr8voh8V10kGhABbNcHVk5wHgWwg==";
      };
    }
    {
      name = "https___registry.npmjs.org_mime_db___mime_db_1.52.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_mime_db___mime_db_1.52.0.tgz";
        url  = "https://registry.npmjs.org/mime-db/-/mime-db-1.52.0.tgz";
        sha512 = "sPU4uV7dYlvtWJxwwxHD0PuihVNiE7TyAbQ5SWxDCB9mUYvOgroQOwYQQOKPJ8CIbE+1ETVlOoK1UC2nU3gYvg==";
      };
    }
    {
      name = "https___registry.npmjs.org_mime_types___mime_types_2.1.35.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_mime_types___mime_types_2.1.35.tgz";
        url  = "https://registry.npmjs.org/mime-types/-/mime-types-2.1.35.tgz";
        sha512 = "ZDY+bPm5zTTF+YpCrAU9nK0UgICYPT0QtT1NZWFv4s++TNkcgVaT0g6+4R2uI4MjQjzysHB1zxuWL50hzaeXiw==";
      };
    }
    {
      name = "https___registry.npmjs.org_mimic_fn___mimic_fn_2.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_mimic_fn___mimic_fn_2.1.0.tgz";
        url  = "https://registry.npmjs.org/mimic-fn/-/mimic-fn-2.1.0.tgz";
        sha512 = "OqbOk5oEQeAZ8WXWydlu9HJjz9WVdEIvamMCcXmuqUYjTknH/sqsWvhQ3vgwKFRR1HpjvNBKQ37nbJgYzGqGcg==";
      };
    }
    {
      name = "https___registry.npmjs.org_mini_svg_data_uri___mini_svg_data_uri_1.4.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_mini_svg_data_uri___mini_svg_data_uri_1.4.4.tgz";
        url  = "https://registry.npmjs.org/mini-svg-data-uri/-/mini-svg-data-uri-1.4.4.tgz";
        sha512 = "r9deDe9p5FJUPZAk3A59wGH7Ii9YrjjWw0jmw/liSbHl2CHiyXj6FcDXDu2K3TjVAXqiJdaw3xxwlZZr9E6nHg==";
      };
    }
    {
      name = "https___registry.npmjs.org_minimatch___minimatch_3.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_minimatch___minimatch_3.1.2.tgz";
        url  = "https://registry.npmjs.org/minimatch/-/minimatch-3.1.2.tgz";
        sha512 = "J7p63hRiAjw1NDEww1W7i37+ByIrOWO5XQQAzZ3VOcL0PNybwpfmV/N05zFAzwQ9USyEcX6t3UO+K5aqBQOIHw==";
      };
    }
    {
      name = "https___registry.npmjs.org_minimist___minimist_1.2.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_minimist___minimist_1.2.6.tgz";
        url  = "https://registry.npmjs.org/minimist/-/minimist-1.2.6.tgz";
        sha512 = "Jsjnk4bw3YJqYzbdyBiNsPWHPfO++UGG749Cxs6peCu5Xg4nrena6OVxOYxrQTqww0Jmwt+Ref8rggumkTLz9Q==";
      };
    }
    {
      name = "https___registry.npmjs.org_mkdirp___mkdirp_0.5.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_mkdirp___mkdirp_0.5.5.tgz";
        url  = "https://registry.npmjs.org/mkdirp/-/mkdirp-0.5.5.tgz";
        sha512 = "NKmAlESf6jMGym1++R0Ra7wvhV+wFW63FaSOFPwRahvea0gMUcGUhVeAg/0BC0wiv9ih5NYPB1Wn1UEI1/L+xQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_ms___ms_2.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ms___ms_2.1.2.tgz";
        url  = "https://registry.npmjs.org/ms/-/ms-2.1.2.tgz";
        sha512 = "sGkPx+VjMtmA6MX27oA4FBFELFCZZ4S4XqeGOXCv68tT+jb3vk/RyaKWP0PTKyWtmLSM0b+adUTEvbs1PEaH2w==";
      };
    }
    {
      name = "https___registry.npmjs.org_nanoid___nanoid_3.3.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_nanoid___nanoid_3.3.1.tgz";
        url  = "https://registry.npmjs.org/nanoid/-/nanoid-3.3.1.tgz";
        sha512 = "n6Vs/3KGyxPQd6uO0eH4Bv0ojGSUvuLlIHtC3Y0kEO23YRge8H9x1GCzLn28YX0H66pMkxuaeESFq4tKISKwdw==";
      };
    }
    {
      name = "https___registry.npmjs.org_natural_compare___natural_compare_1.4.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_natural_compare___natural_compare_1.4.0.tgz";
        url  = "https://registry.npmjs.org/natural-compare/-/natural-compare-1.4.0.tgz";
        sha1 = "Sr6/7tdUHywnrPspvbvRXI1bpPc=";
      };
    }
    {
      name = "https___registry.npmjs.org_node_int64___node_int64_0.4.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_node_int64___node_int64_0.4.0.tgz";
        url  = "https://registry.npmjs.org/node-int64/-/node-int64-0.4.0.tgz";
        sha1 = "h6kGXNs1XTGC2PlM4RGIuCXGijs=";
      };
    }
    {
      name = "https___registry.npmjs.org_node_releases___node_releases_2.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_node_releases___node_releases_2.0.2.tgz";
        url  = "https://registry.npmjs.org/node-releases/-/node-releases-2.0.2.tgz";
        sha512 = "XxYDdcQ6eKqp/YjI+tb2C5WM2LgjnZrfYg4vgQt49EK268b6gYCHsBLrK2qvJo4FmCtqmKezb0WZFK4fkrZNsg==";
      };
    }
    {
      name = "https___registry.npmjs.org_normalize_path___normalize_path_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_normalize_path___normalize_path_3.0.0.tgz";
        url  = "https://registry.npmjs.org/normalize-path/-/normalize-path-3.0.0.tgz";
        sha512 = "6eZs5Ls3WtCisHWp9S2GUy8dqkpGi4BVSz3GaqiE6ezub0512ESztXUwUB6C6IKbQkY2Pnb/mD4WYojCRwcwLA==";
      };
    }
    {
      name = "https___registry.npmjs.org_normalize_range___normalize_range_0.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_normalize_range___normalize_range_0.1.2.tgz";
        url  = "https://registry.npmjs.org/normalize-range/-/normalize-range-0.1.2.tgz";
        sha1 = "LRDAa9/TEuqXd2laTShDlFa3WUI=";
      };
    }
    {
      name = "https___registry.npmjs.org_npm_run_path___npm_run_path_4.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_npm_run_path___npm_run_path_4.0.1.tgz";
        url  = "https://registry.npmjs.org/npm-run-path/-/npm-run-path-4.0.1.tgz";
        sha512 = "S48WzZW777zhNIrn7gxOlISNAqi9ZC/uQFnRdbeIHhZhCA6UqpkOT8T1G7BvfdgP4Er8gF4sUbaS0i7QvIfCWw==";
      };
    }
    {
      name = "https___registry.npmjs.org_nwsapi___nwsapi_2.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_nwsapi___nwsapi_2.2.0.tgz";
        url  = "https://registry.npmjs.org/nwsapi/-/nwsapi-2.2.0.tgz";
        sha512 = "h2AatdwYH+JHiZpv7pt/gSX1XoRGb7L/qSIeuqA6GwYoF9w1vP1cw42TO0aI2pNyshRK5893hNSl+1//vHK7hQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_object_hash___object_hash_2.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_object_hash___object_hash_2.2.0.tgz";
        url  = "https://registry.npmjs.org/object-hash/-/object-hash-2.2.0.tgz";
        sha512 = "gScRMn0bS5fH+IuwyIFgnh9zBdo4DV+6GhygmWM9HyNJSgS0hScp1f5vjtm7oIIOiT9trXrShAkLFSc2IqKNgw==";
      };
    }
    {
      name = "https___registry.npmjs.org_once___once_1.4.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_once___once_1.4.0.tgz";
        url  = "https://registry.npmjs.org/once/-/once-1.4.0.tgz";
        sha1 = "WDsap3WWHUsROsF9nFC6753Xa9E=";
      };
    }
    {
      name = "https___registry.npmjs.org_onetime___onetime_5.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_onetime___onetime_5.1.2.tgz";
        url  = "https://registry.npmjs.org/onetime/-/onetime-5.1.2.tgz";
        sha512 = "kbpaSSGJTWdAY5KPVeMOKXSrPtr8C8C7wodJbcsd51jRnmD+GZu8Y0VoU6Dm5Z4vWr0Ig/1NKuWRKf7j5aaYSg==";
      };
    }
    {
      name = "https___registry.npmjs.org_optionator___optionator_0.8.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_optionator___optionator_0.8.3.tgz";
        url  = "https://registry.npmjs.org/optionator/-/optionator-0.8.3.tgz";
        sha512 = "+IW9pACdk3XWmmTXG8m3upGUJst5XRGzxMRjXzAuJ1XnIFNvfhjjIuYkDvysnPQ7qzqVzLt78BCruntqRhWQbA==";
      };
    }
    {
      name = "https___registry.npmjs.org_os_homedir___os_homedir_1.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_os_homedir___os_homedir_1.0.2.tgz";
        url  = "https://registry.npmjs.org/os-homedir/-/os-homedir-1.0.2.tgz";
        sha1 = "/7xJiDNuDoM94MFox+8VISGqf7M=";
      };
    }
    {
      name = "https___registry.npmjs.org_p_limit___p_limit_2.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_p_limit___p_limit_2.3.0.tgz";
        url  = "https://registry.npmjs.org/p-limit/-/p-limit-2.3.0.tgz";
        sha512 = "//88mFWSJx8lxCzwdAABTJL2MyWB12+eIY7MDL2SqLmAkeKU9qxRvWuSyTjm3FUmpBEMuFfckAIqEaVGUDxb6w==";
      };
    }
    {
      name = "https___registry.npmjs.org_p_locate___p_locate_4.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_p_locate___p_locate_4.1.0.tgz";
        url  = "https://registry.npmjs.org/p-locate/-/p-locate-4.1.0.tgz";
        sha512 = "R79ZZ/0wAxKGu3oYMlz8jy/kbhsNrS7SKZ7PxEHBgJ5+F2mtFW2fK2cOtBh1cHYkQsbzFV7I+EoRKe6Yt0oK7A==";
      };
    }
    {
      name = "https___registry.npmjs.org_p_try___p_try_2.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_p_try___p_try_2.2.0.tgz";
        url  = "https://registry.npmjs.org/p-try/-/p-try-2.2.0.tgz";
        sha512 = "R4nPAVTAU0B9D35/Gk3uJf/7XYbQcyohSKdvAxIRSNghFl4e71hVoGnBNQz9cWaXxO2I10KTC+3jMdvvoKw6dQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_parent_module___parent_module_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_parent_module___parent_module_1.0.1.tgz";
        url  = "https://registry.npmjs.org/parent-module/-/parent-module-1.0.1.tgz";
        sha512 = "GQ2EWRpQV8/o+Aw8YqtfZZPfNRWZYkbidE9k5rpl/hC3vtHHBfGm2Ifi6qWV+coDGkrUKZAxE3Lot5kcsRlh+g==";
      };
    }
    {
      name = "https___registry.npmjs.org_parse_json___parse_json_5.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_parse_json___parse_json_5.2.0.tgz";
        url  = "https://registry.npmjs.org/parse-json/-/parse-json-5.2.0.tgz";
        sha512 = "ayCKvm/phCGxOkYRSCM82iDwct8/EonSEgCSxWxD7ve6jHggsFl4fZVQBPRNgQoKiuV/odhFrGzQXZwbifC8Rg==";
      };
    }
    {
      name = "https___registry.npmjs.org_parse_passwd___parse_passwd_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_parse_passwd___parse_passwd_1.0.0.tgz";
        url  = "https://registry.npmjs.org/parse-passwd/-/parse-passwd-1.0.0.tgz";
        sha1 = "bVuTSkVpk7I9N/QKOC1vFmao5cY=";
      };
    }
    {
      name = "https___registry.npmjs.org_parse5___parse5_6.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_parse5___parse5_6.0.1.tgz";
        url  = "https://registry.npmjs.org/parse5/-/parse5-6.0.1.tgz";
        sha512 = "Ofn/CTFzRGTTxwpNEs9PP93gXShHcTq255nzRYSKe8AkVpZY7e1fpmTfOyoIvjP5HG7Z2ZM7VS9PPhQGW2pOpw==";
      };
    }
    {
      name = "https___registry.npmjs.org_path_exists___path_exists_4.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_path_exists___path_exists_4.0.0.tgz";
        url  = "https://registry.npmjs.org/path-exists/-/path-exists-4.0.0.tgz";
        sha512 = "ak9Qy5Q7jYb2Wwcey5Fpvg2KoAc/ZIhLSLOSBmRmygPsGwkVVt0fZa0qrtMz+m6tJTAHfZQ8FnmB4MG4LWy7/w==";
      };
    }
    {
      name = "https___registry.npmjs.org_path_is_absolute___path_is_absolute_1.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_path_is_absolute___path_is_absolute_1.0.1.tgz";
        url  = "https://registry.npmjs.org/path-is-absolute/-/path-is-absolute-1.0.1.tgz";
        sha1 = "F0uSaHNVNP+8es5r9TpanhtcX18=";
      };
    }
    {
      name = "https___registry.npmjs.org_path_key___path_key_3.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_path_key___path_key_3.1.1.tgz";
        url  = "https://registry.npmjs.org/path-key/-/path-key-3.1.1.tgz";
        sha512 = "ojmeN0qd+y0jszEtoY48r0Peq5dwMEkIlCOu6Q5f41lfkswXuKtYrhgoTpLnyIcHm24Uhqx+5Tqm2InSwLhE6Q==";
      };
    }
    {
      name = "https___registry.npmjs.org_path_parse___path_parse_1.0.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_path_parse___path_parse_1.0.7.tgz";
        url  = "https://registry.npmjs.org/path-parse/-/path-parse-1.0.7.tgz";
        sha512 = "LDJzPVEEEPR+y48z93A0Ed0yXb8pAByGWo/k5YYdYgpY2/2EsOsksJrq7lOHxryrVOn1ejG6oAp8ahvOIQD8sw==";
      };
    }
    {
      name = "https___registry.npmjs.org_path_type___path_type_4.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_path_type___path_type_4.0.0.tgz";
        url  = "https://registry.npmjs.org/path-type/-/path-type-4.0.0.tgz";
        sha512 = "gDKb8aZMDeD/tZWs9P6+q0J9Mwkdl6xMV8TjnGP3qJVJ06bdMgkbBlLU8IdfOsIsFz2BW1rNVT3XuNEl8zPAvw==";
      };
    }
    {
      name = "https___registry.npmjs.org_picocolors___picocolors_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_picocolors___picocolors_1.0.0.tgz";
        url  = "https://registry.npmjs.org/picocolors/-/picocolors-1.0.0.tgz";
        sha512 = "1fygroTLlHu66zi26VoTDv8yRgm0Fccecssto+MhsZ0D/DGW2sm8E8AjW7NU5VVTRt5GxbeZ5qBuJr+HyLYkjQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_picomatch___picomatch_2.3.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_picomatch___picomatch_2.3.1.tgz";
        url  = "https://registry.npmjs.org/picomatch/-/picomatch-2.3.1.tgz";
        sha512 = "JU3teHTNjmE2VCGFzuY8EXzCDVwEqB2a8fsIvwaStHhAWJEeVd1o1QD80CU6+ZdEXXSLbSsuLwJjkCBWqRQUVA==";
      };
    }
    {
      name = "https___registry.npmjs.org_pirates___pirates_4.0.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_pirates___pirates_4.0.5.tgz";
        url  = "https://registry.npmjs.org/pirates/-/pirates-4.0.5.tgz";
        sha512 = "8V9+HQPupnaXMA23c5hvl69zXvTwTzyAYasnkb0Tts4XvO4CliqONMOnvlq26rkhLC3nWDFBJf73LU1e1VZLaQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_pkg_dir___pkg_dir_4.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_pkg_dir___pkg_dir_4.2.0.tgz";
        url  = "https://registry.npmjs.org/pkg-dir/-/pkg-dir-4.2.0.tgz";
        sha512 = "HRDzbaKjC+AOWVXxAU/x54COGeIv9eb+6CkDSQoNTt4XyWoIJvuPsXizxu/Fr23EiekbtZwmh1IcIG/l/a10GQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_js___postcss_js_4.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_js___postcss_js_4.0.0.tgz";
        url  = "https://registry.npmjs.org/postcss-js/-/postcss-js-4.0.0.tgz";
        sha512 = "77QESFBwgX4irogGVPgQ5s07vLvFqWr228qZY+w6lW599cRlK/HmnlivnnVUxkjHnCu4J16PDMHcH+e+2HbvTQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_load_config___postcss_load_config_3.1.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_load_config___postcss_load_config_3.1.3.tgz";
        url  = "https://registry.npmjs.org/postcss-load-config/-/postcss-load-config-3.1.3.tgz";
        sha512 = "5EYgaM9auHGtO//ljHH+v/aC/TQ5LHXtL7bQajNAUBKUVKiYE8rYpFms7+V26D9FncaGe2zwCoPQsFKb5zF/Hw==";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_modules_extract_imports___postcss_modules_extract_imports_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_modules_extract_imports___postcss_modules_extract_imports_3.0.0.tgz";
        url  = "https://registry.npmjs.org/postcss-modules-extract-imports/-/postcss-modules-extract-imports-3.0.0.tgz";
        sha512 = "bdHleFnP3kZ4NYDhuGlVK+CMrQ/pqUm8bx/oGL93K6gVwiclvX5x0n76fYMKuIGKzlABOy13zsvqjb0f92TEXw==";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_modules_local_by_default___postcss_modules_local_by_default_4.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_modules_local_by_default___postcss_modules_local_by_default_4.0.0.tgz";
        url  = "https://registry.npmjs.org/postcss-modules-local-by-default/-/postcss-modules-local-by-default-4.0.0.tgz";
        sha512 = "sT7ihtmGSF9yhm6ggikHdV0hlziDTX7oFoXtuVWeDd3hHObNkcHRo9V3yg7vCAY7cONyxJC/XXCmmiHHcvX7bQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_modules_scope___postcss_modules_scope_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_modules_scope___postcss_modules_scope_3.0.0.tgz";
        url  = "https://registry.npmjs.org/postcss-modules-scope/-/postcss-modules-scope-3.0.0.tgz";
        sha512 = "hncihwFA2yPath8oZ15PZqvWGkWf+XUfQgUGamS4LqoP1anQLOsOJw0vr7J7IwLpoY9fatA2qiGUGmuZL0Iqlg==";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_modules_values___postcss_modules_values_4.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_modules_values___postcss_modules_values_4.0.0.tgz";
        url  = "https://registry.npmjs.org/postcss-modules-values/-/postcss-modules-values-4.0.0.tgz";
        sha512 = "RDxHkAiEGI78gS2ofyvCsu7iycRv7oqw5xMWn9iMoR0N/7mf9D50ecQqUo5BZ9Zh2vH4bCUR/ktCqbB9m8vJjQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_modules___postcss_modules_4.3.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_modules___postcss_modules_4.3.1.tgz";
        url  = "https://registry.npmjs.org/postcss-modules/-/postcss-modules-4.3.1.tgz";
        sha512 = "ItUhSUxBBdNamkT3KzIZwYNNRFKmkJrofvC2nWab3CPKhYBQ1f27XXh1PAPE27Psx58jeelPsxWB/+og+KEH0Q==";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_nested___postcss_nested_5.0.6.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_nested___postcss_nested_5.0.6.tgz";
        url  = "https://registry.npmjs.org/postcss-nested/-/postcss-nested-5.0.6.tgz";
        sha512 = "rKqm2Fk0KbA8Vt3AdGN0FB9OBOMDVajMG6ZCf/GoHgdxUJ4sBFp0A/uMIRm+MJUdo33YXEtjqIz8u7DAp8B7DA==";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_selector_parser___postcss_selector_parser_6.0.9.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_selector_parser___postcss_selector_parser_6.0.9.tgz";
        url  = "https://registry.npmjs.org/postcss-selector-parser/-/postcss-selector-parser-6.0.9.tgz";
        sha512 = "UO3SgnZOVTwu4kyLR22UQ1xZh086RyNZppb7lLAKBFK8a32ttG5i87Y/P3+2bRSjZNyJ1B7hfFNo273tKe9YxQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss_value_parser___postcss_value_parser_4.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss_value_parser___postcss_value_parser_4.2.0.tgz";
        url  = "https://registry.npmjs.org/postcss-value-parser/-/postcss-value-parser-4.2.0.tgz";
        sha512 = "1NNCs6uurfkVbeXG4S8JFT9t19m45ICnif8zWLd5oPSZ50QnwMfK+H3jv408d4jw/7Bttv5axS5IiHoLaVNHeQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_postcss___postcss_8.4.8.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_postcss___postcss_8.4.8.tgz";
        url  = "https://registry.npmjs.org/postcss/-/postcss-8.4.8.tgz";
        sha512 = "2tXEqGxrjvAO6U+CJzDL2Fk2kPHTv1jQsYkSoMeOis2SsYaXRO2COxTdQp99cYvif9JTXaAk9lYGc3VhJt7JPQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_prelude_ls___prelude_ls_1.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_prelude_ls___prelude_ls_1.1.2.tgz";
        url  = "https://registry.npmjs.org/prelude-ls/-/prelude-ls-1.1.2.tgz";
        sha1 = "IZMqVJ9eUv/ZqCf1cOBL5iqX2lQ=";
      };
    }
    {
      name = "https___registry.npmjs.org_pretty_format___pretty_format_27.5.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_pretty_format___pretty_format_27.5.1.tgz";
        url  = "https://registry.npmjs.org/pretty-format/-/pretty-format-27.5.1.tgz";
        sha512 = "Qb1gy5OrP5+zDf2Bvnzdl3jsTf1qXVMazbvCoKhtKqVs4/YK4ozX4gKQJJVyNe+cajNPn0KoC0MC3FUmaHWEmQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_prompts___prompts_2.4.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_prompts___prompts_2.4.2.tgz";
        url  = "https://registry.npmjs.org/prompts/-/prompts-2.4.2.tgz";
        sha512 = "NxNv/kLguCA7p3jE8oL2aEBsrJWgAakBpgmgK6lpPWV+WuOmY6r2/zbAVnP+T8bQlA0nzHXSJSJW0Hq7ylaD2Q==";
      };
    }
    {
      name = "https___registry.npmjs.org_psl___psl_1.8.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_psl___psl_1.8.0.tgz";
        url  = "https://registry.npmjs.org/psl/-/psl-1.8.0.tgz";
        sha512 = "RIdOzyoavK+hA18OGGWDqUTsCLhtA7IcZ/6NCs4fFJaHBDab+pDDmDIByWFRQJq2Cd7r1OoQxBGKOaztq+hjIQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_punycode___punycode_2.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_punycode___punycode_2.1.1.tgz";
        url  = "https://registry.npmjs.org/punycode/-/punycode-2.1.1.tgz";
        sha512 = "XRsRjdf+j5ml+y/6GKHPZbrF/8p2Yga0JPtdqTIY2Xe5ohJPD9saDJJLPvp9+NSBprVvevdXZybnj2cv8OEd0A==";
      };
    }
    {
      name = "https___registry.npmjs.org_queue_microtask___queue_microtask_1.2.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_queue_microtask___queue_microtask_1.2.3.tgz";
        url  = "https://registry.npmjs.org/queue-microtask/-/queue-microtask-1.2.3.tgz";
        sha512 = "NuaNSa6flKT5JaSYQzJok04JzTL1CA6aGhv5rfLW3PgqA+M2ChpZQnAC8h8i4ZFkBS8X5RqkDBHA7r4hej3K9A==";
      };
    }
    {
      name = "https___registry.npmjs.org_quick_lru___quick_lru_5.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_quick_lru___quick_lru_5.1.1.tgz";
        url  = "https://registry.npmjs.org/quick-lru/-/quick-lru-5.1.1.tgz";
        sha512 = "WuyALRjWPDGtt/wzJiadO5AXY+8hZ80hVpe6MyivgraREW751X3SbhRvG3eLKOYN+8VEvqLcf3wdnt44Z4S4SA==";
      };
    }
    {
      name = "https___registry.npmjs.org_react_is___react_is_17.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_react_is___react_is_17.0.2.tgz";
        url  = "https://registry.npmjs.org/react-is/-/react-is-17.0.2.tgz";
        sha512 = "w2GsyukL62IJnlaff/nRegPQR94C/XXamvMWmSHRJ4y7Ts/4ocGRmTHvOs8PSE6pB3dWOrD/nueuU5sduBsQ4w==";
      };
    }
    {
      name = "https___registry.npmjs.org_readdirp___readdirp_3.6.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_readdirp___readdirp_3.6.0.tgz";
        url  = "https://registry.npmjs.org/readdirp/-/readdirp-3.6.0.tgz";
        sha512 = "hOS089on8RduqdbhvQ5Z37A0ESjsqz6qnRcffsMU3495FuTdqSm+7bhJ29JvIOsBDEEnan5DPu9t3To9VRlMzA==";
      };
    }
    {
      name = "https___registry.npmjs.org_require_directory___require_directory_2.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_require_directory___require_directory_2.1.1.tgz";
        url  = "https://registry.npmjs.org/require-directory/-/require-directory-2.1.1.tgz";
        sha1 = "jGStX9MNqxyXbiNE/+f3kqam30I=";
      };
    }
    {
      name = "https___registry.npmjs.org_resolve_cwd___resolve_cwd_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_resolve_cwd___resolve_cwd_3.0.0.tgz";
        url  = "https://registry.npmjs.org/resolve-cwd/-/resolve-cwd-3.0.0.tgz";
        sha512 = "OrZaX2Mb+rJCpH/6CpSqt9xFVpN++x01XnN2ie9g6P5/3xelLAkXWVADpdz1IHD/KFfEXyE6V0U01OQ3UO2rEg==";
      };
    }
    {
      name = "https___registry.npmjs.org_resolve_dir___resolve_dir_0.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_resolve_dir___resolve_dir_0.1.1.tgz";
        url  = "https://registry.npmjs.org/resolve-dir/-/resolve-dir-0.1.1.tgz";
        sha1 = "shklmlYC+sXFxJatiUpujMQwJh4=";
      };
    }
    {
      name = "https___registry.npmjs.org_resolve_file___resolve_file_0.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_resolve_file___resolve_file_0.3.0.tgz";
        url  = "https://registry.npmjs.org/resolve-file/-/resolve-file-0.3.0.tgz";
        sha1 = "EeH7RkVm06fFAMt+lIHo8LAKFO8=";
      };
    }
    {
      name = "https___registry.npmjs.org_resolve_from___resolve_from_4.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_resolve_from___resolve_from_4.0.0.tgz";
        url  = "https://registry.npmjs.org/resolve-from/-/resolve-from-4.0.0.tgz";
        sha512 = "pb/MYmXstAkysRFx8piNI1tGFNQIFA3vkE3Gq4EuA1dF6gHp/+vgZqsCGJapvy8N3Q+4o7FwvquPJcnZ7RYy4g==";
      };
    }
    {
      name = "https___registry.npmjs.org_resolve_from___resolve_from_5.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_resolve_from___resolve_from_5.0.0.tgz";
        url  = "https://registry.npmjs.org/resolve-from/-/resolve-from-5.0.0.tgz";
        sha512 = "qYg9KP24dD5qka9J47d0aVky0N+b4fTU89LN9iDnjB5waksiC49rvMB0PrUJQGoTmH50XPiqOvAjDfaijGxYZw==";
      };
    }
    {
      name = "https___registry.npmjs.org_resolve.exports___resolve.exports_1.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_resolve.exports___resolve.exports_1.1.0.tgz";
        url  = "https://registry.npmjs.org/resolve.exports/-/resolve.exports-1.1.0.tgz";
        sha512 = "J1l+Zxxp4XK3LUDZ9m60LRJF/mAe4z6a4xyabPHk7pvK5t35dACV32iIjJDFeWZFfZlO29w6SZ67knR0tHzJtQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_resolve___resolve_1.22.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_resolve___resolve_1.22.0.tgz";
        url  = "https://registry.npmjs.org/resolve/-/resolve-1.22.0.tgz";
        sha512 = "Hhtrw0nLeSrFQ7phPp4OOcVjLPIeMnRlr5mcnVuMe7M/7eBn98A3hmFRLoFo3DLZkivSYwhRUJTyPyWAk56WLw==";
      };
    }
    {
      name = "https___registry.npmjs.org_reusify___reusify_1.0.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_reusify___reusify_1.0.4.tgz";
        url  = "https://registry.npmjs.org/reusify/-/reusify-1.0.4.tgz";
        sha512 = "U9nH88a3fc/ekCF1l0/UP1IosiuIjyTh7hBvXVMHYgVcfGvt897Xguj2UOLDeI5BG2m7/uwyaLVT6fbtCwTyzw==";
      };
    }
    {
      name = "https___registry.npmjs.org_rimraf___rimraf_3.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_rimraf___rimraf_3.0.2.tgz";
        url  = "https://registry.npmjs.org/rimraf/-/rimraf-3.0.2.tgz";
        sha512 = "JZkJMZkAGFFPP2YqXZXPbMlMBgsxzE8ILs4lMIX/2o0L9UBw9O/Y3o6wFw/i9YLapcUJWwqbi3kdxIPdC62TIA==";
      };
    }
    {
      name = "https___registry.npmjs.org_rimraf___rimraf_2.6.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_rimraf___rimraf_2.6.3.tgz";
        url  = "https://registry.npmjs.org/rimraf/-/rimraf-2.6.3.tgz";
        sha512 = "mwqeW5XsA2qAejG46gYdENaxXjx9onRNCfn7L0duuP4hCuTIi/QO7PDK07KJfp1d+izWPrzEJDcSqBa0OZQriA==";
      };
    }
    {
      name = "https___registry.npmjs.org_run_parallel___run_parallel_1.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_run_parallel___run_parallel_1.2.0.tgz";
        url  = "https://registry.npmjs.org/run-parallel/-/run-parallel-1.2.0.tgz";
        sha512 = "5l4VyZR86LZ/lDxZTR6jqL8AFE2S0IFLMP26AbjsLVADxHdhB/c0GUsH+y39UfCi3dzz8OlQuPmnaJOMoDHQBA==";
      };
    }
    {
      name = "https___registry.npmjs.org_safe_buffer___safe_buffer_5.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_safe_buffer___safe_buffer_5.1.2.tgz";
        url  = "https://registry.npmjs.org/safe-buffer/-/safe-buffer-5.1.2.tgz";
        sha512 = "Gd2UZBJDkXlY7GbJxfsE8/nvKkUEU1G38c1siN6QP6a9PT9MmHB8GnpscSmMJSoF8LOIrt8ud/wPtojys4G6+g==";
      };
    }
    {
      name = "https___registry.npmjs.org_safer_buffer___safer_buffer_2.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_safer_buffer___safer_buffer_2.1.2.tgz";
        url  = "https://registry.npmjs.org/safer-buffer/-/safer-buffer-2.1.2.tgz";
        sha512 = "YZo3K82SD7Riyi0E1EQPojLz7kpepnSQI9IyPbHHg1XXXevb5dJI7tpyN2ADxGcQbHG7vcyRHk0cbwqcQriUtg==";
      };
    }
    {
      name = "https___registry.npmjs.org_saxes___saxes_5.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_saxes___saxes_5.0.1.tgz";
        url  = "https://registry.npmjs.org/saxes/-/saxes-5.0.1.tgz";
        sha512 = "5LBh1Tls8c9xgGjw3QrMwETmTMVk0oFgvrFSvWx62llR2hcEInrKNZ2GZCCuuy2lvWrdl5jhbpeqc5hRYKFOcw==";
      };
    }
    {
      name = "https___registry.npmjs.org_semver___semver_7.3.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_semver___semver_7.3.5.tgz";
        url  = "https://registry.npmjs.org/semver/-/semver-7.3.5.tgz";
        sha512 = "PoeGJYh8HK4BTO/a9Tf6ZG3veo/A7ZVsYrSA6J8ny9nb3B1VrpkuN+z9OE5wfE5p6H4LchYZsegiQgbJD94ZFQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_semver___semver_6.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_semver___semver_6.3.0.tgz";
        url  = "https://registry.npmjs.org/semver/-/semver-6.3.0.tgz";
        sha512 = "b39TBaTSfV6yBrapU89p5fKekE2m/NwnDocOVruQFS1/veMgdzuPcnOM34M6CwxW8jH/lxEa5rBoDeUwu5HHTw==";
      };
    }
    {
      name = "https___registry.npmjs.org_set_getter___set_getter_0.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_set_getter___set_getter_0.1.1.tgz";
        url  = "https://registry.npmjs.org/set-getter/-/set-getter-0.1.1.tgz";
        sha512 = "9sVWOy+gthr+0G9DzqqLaYNA7+5OKkSmcqjL9cBpDEaZrr3ShQlyX2cZ/O/ozE41oxn/Tt0LGEM/w4Rub3A3gw==";
      };
    }
    {
      name = "https___registry.npmjs.org_shebang_command___shebang_command_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_shebang_command___shebang_command_2.0.0.tgz";
        url  = "https://registry.npmjs.org/shebang-command/-/shebang-command-2.0.0.tgz";
        sha512 = "kHxr2zZpYtdmrN1qDjrrX/Z1rR1kG8Dx+gkpK1G4eXmvXswmcE1hTWBWYUzlraYw1/yZp6YuDY77YtvbN0dmDA==";
      };
    }
    {
      name = "https___registry.npmjs.org_shebang_regex___shebang_regex_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_shebang_regex___shebang_regex_3.0.0.tgz";
        url  = "https://registry.npmjs.org/shebang-regex/-/shebang-regex-3.0.0.tgz";
        sha512 = "7++dFhtcx3353uBaq8DDR4NuxBetBzC7ZQOhmTQInHEd6bSrXdiEyzCvG07Z44UYdLShWUyXt5M/yhz8ekcb1A==";
      };
    }
    {
      name = "https___registry.npmjs.org_signal_exit___signal_exit_3.0.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_signal_exit___signal_exit_3.0.7.tgz";
        url  = "https://registry.npmjs.org/signal-exit/-/signal-exit-3.0.7.tgz";
        sha512 = "wnD2ZE+l+SPC/uoS0vXeE9L1+0wuaMqKlfz9AMUo38JsyLSBWSFcHR1Rri62LZc12vLr1gb3jl7iwQhgwpAbGQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_sisteransi___sisteransi_1.0.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_sisteransi___sisteransi_1.0.5.tgz";
        url  = "https://registry.npmjs.org/sisteransi/-/sisteransi-1.0.5.tgz";
        sha512 = "bLGGlR1QxBcynn2d5YmDX4MGjlZvy2MRBDRNHLJ8VI6l6+9FUiyTFNJ0IveOSP0bcXgVDPRcfGqA0pjaqUpfVg==";
      };
    }
    {
      name = "https___registry.npmjs.org_slash___slash_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_slash___slash_3.0.0.tgz";
        url  = "https://registry.npmjs.org/slash/-/slash-3.0.0.tgz";
        sha512 = "g9Q1haeby36OSStwb4ntCGGGaKsaVSjQ68fBxoQcutl5fS1vuY18H3wSt3jFyFtrkx+Kz0V1G85A4MyAdDMi2Q==";
      };
    }
    {
      name = "https___registry.npmjs.org_source_map_js___source_map_js_1.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_source_map_js___source_map_js_1.0.2.tgz";
        url  = "https://registry.npmjs.org/source-map-js/-/source-map-js-1.0.2.tgz";
        sha512 = "R0XvVJ9WusLiqTCEiGCmICCMplcCkIwwR11mOSD9CR5u+IXYdiseeEuXCVAjS54zqwkLcPNnmU4OeJ6tUrWhDw==";
      };
    }
    {
      name = "https___registry.npmjs.org_source_map_support___source_map_support_0.5.21.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_source_map_support___source_map_support_0.5.21.tgz";
        url  = "https://registry.npmjs.org/source-map-support/-/source-map-support-0.5.21.tgz";
        sha512 = "uBHU3L3czsIyYXKX88fdrGovxdSCoTGDRZ6SYXtSRxLZUzHg5P/66Ht6uoUlHu9EZod+inXhKo3qQgwXUT/y1w==";
      };
    }
    {
      name = "https___registry.npmjs.org_source_map___source_map_0.5.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_source_map___source_map_0.5.7.tgz";
        url  = "https://registry.npmjs.org/source-map/-/source-map-0.5.7.tgz";
        sha1 = "igOdLRAh0i0eoUyA2OpGi6LvP8w=";
      };
    }
    {
      name = "https___registry.npmjs.org_source_map___source_map_0.6.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_source_map___source_map_0.6.1.tgz";
        url  = "https://registry.npmjs.org/source-map/-/source-map-0.6.1.tgz";
        sha512 = "UjgapumWlbMhkBgzT7Ykc5YXUT46F0iKu8SGXq0bcwP5dz/h0Plj6enJqjz1Zbq2l5WaqYnrVbwWOWMyF3F47g==";
      };
    }
    {
      name = "https___registry.npmjs.org_source_map___source_map_0.7.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_source_map___source_map_0.7.3.tgz";
        url  = "https://registry.npmjs.org/source-map/-/source-map-0.7.3.tgz";
        sha512 = "CkCj6giN3S+n9qrYiBTX5gystlENnRW5jZeNLHpe6aue+SrHcG5VYwujhW9s4dY31mEGsxBDrHR6oI69fTXsaQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_sprintf_js___sprintf_js_1.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_sprintf_js___sprintf_js_1.0.3.tgz";
        url  = "https://registry.npmjs.org/sprintf-js/-/sprintf-js-1.0.3.tgz";
        sha1 = "BOaSb2YolTVPPdAVIDYzuFcpfiw=";
      };
    }
    {
      name = "https___registry.npmjs.org_stack_utils___stack_utils_2.0.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_stack_utils___stack_utils_2.0.5.tgz";
        url  = "https://registry.npmjs.org/stack-utils/-/stack-utils-2.0.5.tgz";
        sha512 = "xrQcmYhOsn/1kX+Vraq+7j4oE2j/6BFscZ0etmYg81xuM8Gq0022Pxb8+IqgOFUIaxHs0KaSb7T1+OegiNrNFA==";
      };
    }
    {
      name = "https___registry.npmjs.org_string_hash___string_hash_1.1.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_string_hash___string_hash_1.1.3.tgz";
        url  = "https://registry.npmjs.org/string-hash/-/string-hash-1.1.3.tgz";
        sha1 = "6Kr8CsGFW0Zmkp7X3RJ1311sgRs=";
      };
    }
    {
      name = "https___registry.npmjs.org_string_length___string_length_4.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_string_length___string_length_4.0.2.tgz";
        url  = "https://registry.npmjs.org/string-length/-/string-length-4.0.2.tgz";
        sha512 = "+l6rNN5fYHNhZZy41RXsYptCjA2Igmq4EG7kZAYFQI1E1VTXarr6ZPXBg6eq7Y6eK4FEhY6AJlyuFIb/v/S0VQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_string_width___string_width_4.2.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_string_width___string_width_4.2.3.tgz";
        url  = "https://registry.npmjs.org/string-width/-/string-width-4.2.3.tgz";
        sha512 = "wKyQRQpjJ0sIp62ErSZdGsjMJWsap5oRNihHhu6G7JVO/9jIB6UyevL+tXuOqrng8j/cxKTWyWUwvSTriiZz/g==";
      };
    }
    {
      name = "https___registry.npmjs.org_strip_ansi___strip_ansi_6.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_strip_ansi___strip_ansi_6.0.1.tgz";
        url  = "https://registry.npmjs.org/strip-ansi/-/strip-ansi-6.0.1.tgz";
        sha512 = "Y38VPSHcqkFrCpFnQ9vuSXmquuv5oXOKpGeT6aGrr3o3Gc9AlVa6JBfUSOCnbxGGZF+/0ooI7KrPuUSztUdU5A==";
      };
    }
    {
      name = "https___registry.npmjs.org_strip_bom___strip_bom_4.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_strip_bom___strip_bom_4.0.0.tgz";
        url  = "https://registry.npmjs.org/strip-bom/-/strip-bom-4.0.0.tgz";
        sha512 = "3xurFv5tEgii33Zi8Jtp55wEIILR9eh34FAW00PZf+JnSsTmV/ioewSgQl97JHvgjoRGwPShsWm+IdrxB35d0w==";
      };
    }
    {
      name = "https___registry.npmjs.org_strip_final_newline___strip_final_newline_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_strip_final_newline___strip_final_newline_2.0.0.tgz";
        url  = "https://registry.npmjs.org/strip-final-newline/-/strip-final-newline-2.0.0.tgz";
        sha512 = "BrpvfNAE3dcvq7ll3xVumzjKjZQ5tI1sEUIKr3Uoks0XUl45St3FlatVqef9prk4jRDzhW6WZg+3bk93y6pLjA==";
      };
    }
    {
      name = "https___registry.npmjs.org_strip_json_comments___strip_json_comments_3.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_strip_json_comments___strip_json_comments_3.1.1.tgz";
        url  = "https://registry.npmjs.org/strip-json-comments/-/strip-json-comments-3.1.1.tgz";
        sha512 = "6fPc+R4ihwqP6N/aIv2f1gMH8lOVtWQHoqC4yK6oSDVVocumAsfCqjkXnqiYMhmMwS/mEHLp7Vehlt3ql6lEig==";
      };
    }
    {
      name = "https___registry.npmjs.org_style_mod___style_mod_4.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_style_mod___style_mod_4.0.0.tgz";
        url  = "https://registry.npmjs.org/style-mod/-/style-mod-4.0.0.tgz";
        sha512 = "OPhtyEjyyN9x3nhPsu76f52yUGXiZcgvsrFVtvTkyGRQJ0XK+GPc6ov1z+lRpbeabka+MYEQxOYRnt5nF30aMw==";
      };
    }
    {
      name = "https___registry.npmjs.org_supports_color___supports_color_5.5.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_supports_color___supports_color_5.5.0.tgz";
        url  = "https://registry.npmjs.org/supports-color/-/supports-color-5.5.0.tgz";
        sha512 = "QjVjwdXIt408MIiAqCX4oUKsgU2EqAGzs2Ppkm4aQYbjm+ZEWEcW4SfFNTr4uMNZma0ey4f5lgLrkB0aX0QMow==";
      };
    }
    {
      name = "https___registry.npmjs.org_supports_color___supports_color_7.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_supports_color___supports_color_7.2.0.tgz";
        url  = "https://registry.npmjs.org/supports-color/-/supports-color-7.2.0.tgz";
        sha512 = "qpCAvRl9stuOHveKsn7HncJRvv501qIacKzQlO/+Lwxc9+0q2wLyv4Dfvt80/DPn2pqOBsJdDiogXGR9+OvwRw==";
      };
    }
    {
      name = "https___registry.npmjs.org_supports_color___supports_color_8.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_supports_color___supports_color_8.1.1.tgz";
        url  = "https://registry.npmjs.org/supports-color/-/supports-color-8.1.1.tgz";
        sha512 = "MpUEN2OodtUzxvKQl72cUF7RQ5EiHsGvSsVG0ia9c5RbWGL2CI4C7EpPS8UTBIplnlzZiNuV56w+FuNxy3ty2Q==";
      };
    }
    {
      name = "https___registry.npmjs.org_supports_hyperlinks___supports_hyperlinks_2.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_supports_hyperlinks___supports_hyperlinks_2.2.0.tgz";
        url  = "https://registry.npmjs.org/supports-hyperlinks/-/supports-hyperlinks-2.2.0.tgz";
        sha512 = "6sXEzV5+I5j8Bmq9/vUphGRM/RJNT9SCURJLjwfOg51heRtguGWDzcaBlgAzKhQa0EVNpPEKzQuBwZ8S8WaCeQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_supports_preserve_symlinks_flag___supports_preserve_symlinks_flag_1.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_supports_preserve_symlinks_flag___supports_preserve_symlinks_flag_1.0.0.tgz";
        url  = "https://registry.npmjs.org/supports-preserve-symlinks-flag/-/supports-preserve-symlinks-flag-1.0.0.tgz";
        sha512 = "ot0WnXS9fgdkgIcePe6RHNk1WA8+muPa6cSjeR3V8K27q9BB1rTE3R1p7Hv0z1ZyAc8s6Vvv8DIyWf681MAt0w==";
      };
    }
    {
      name = "https___registry.npmjs.org_symbol_tree___symbol_tree_3.2.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_symbol_tree___symbol_tree_3.2.4.tgz";
        url  = "https://registry.npmjs.org/symbol-tree/-/symbol-tree-3.2.4.tgz";
        sha512 = "9QNk5KwDF+Bvz+PyObkmSYjI5ksVUYtjW7AU22r2NKcfLJcXp96hkDWU3+XndOsUb+AQ9QhfzfCT2O+CNWT5Tw==";
      };
    }
    {
      name = "https___registry.npmjs.org_tailwindcss___tailwindcss_3.0.23.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_tailwindcss___tailwindcss_3.0.23.tgz";
        url  = "https://registry.npmjs.org/tailwindcss/-/tailwindcss-3.0.23.tgz";
        sha512 = "+OZOV9ubyQ6oI2BXEhzw4HrqvgcARY38xv3zKcjnWtMIZstEsXdI9xftd1iB7+RbOnj2HOEzkA0OyB5BaSxPQA==";
      };
    }
    {
      name = "https___registry.npmjs.org_temp___temp_0.9.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_temp___temp_0.9.4.tgz";
        url  = "https://registry.npmjs.org/temp/-/temp-0.9.4.tgz";
        sha512 = "yYrrsWnrXMcdsnu/7YMYAofM1ktpL5By7vZhf15CrXijWWrEYZks5AXBudalfSWJLlnen/QUJUB5aoB0kqZUGA==";
      };
    }
    {
      name = "https___registry.npmjs.org_terminal_link___terminal_link_2.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_terminal_link___terminal_link_2.1.1.tgz";
        url  = "https://registry.npmjs.org/terminal-link/-/terminal-link-2.1.1.tgz";
        sha512 = "un0FmiRUQNr5PJqy9kP7c40F5BOfpGlYTrxonDChEZB7pzZxRNp/bt+ymiy9/npwXya9KH99nJ/GXFIiUkYGFQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_test_exclude___test_exclude_6.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_test_exclude___test_exclude_6.0.0.tgz";
        url  = "https://registry.npmjs.org/test-exclude/-/test-exclude-6.0.0.tgz";
        sha512 = "cAGWPIyOHU6zlmg88jwm7VRyXnMN7iV68OGAbYDk/Mh/xC/pzVPlQtY6ngoIH/5/tciuhGfvESU8GrHrcxD56w==";
      };
    }
    {
      name = "https___registry.npmjs.org_throat___throat_6.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_throat___throat_6.0.1.tgz";
        url  = "https://registry.npmjs.org/throat/-/throat-6.0.1.tgz";
        sha512 = "8hmiGIJMDlwjg7dlJ4yKGLK8EsYqKgPWbG3b4wjJddKNwc7N7Dpn08Df4szr/sZdMVeOstrdYSsqzX6BYbcB+w==";
      };
    }
    {
      name = "https___registry.npmjs.org_tmpl___tmpl_1.0.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_tmpl___tmpl_1.0.5.tgz";
        url  = "https://registry.npmjs.org/tmpl/-/tmpl-1.0.5.tgz";
        sha512 = "3f0uOEAQwIqGuWW2MVzYg8fV/QNnc/IpuJNG837rLuczAaLVHslWHZQj4IGiEl5Hs3kkbhwL9Ab7Hrsmuj+Smw==";
      };
    }
    {
      name = "https___registry.npmjs.org_to_fast_properties___to_fast_properties_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_to_fast_properties___to_fast_properties_2.0.0.tgz";
        url  = "https://registry.npmjs.org/to-fast-properties/-/to-fast-properties-2.0.0.tgz";
        sha1 = "3F5pjL0HkmW8c+A3doGk5Og/YW4=";
      };
    }
    {
      name = "https___registry.npmjs.org_to_object_path___to_object_path_0.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_to_object_path___to_object_path_0.3.0.tgz";
        url  = "https://registry.npmjs.org/to-object-path/-/to-object-path-0.3.0.tgz";
        sha1 = "KXWIt7Dn4KwI4E5nL4XB9JmeF68=";
      };
    }
    {
      name = "https___registry.npmjs.org_to_regex_range___to_regex_range_5.0.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_to_regex_range___to_regex_range_5.0.1.tgz";
        url  = "https://registry.npmjs.org/to-regex-range/-/to-regex-range-5.0.1.tgz";
        sha512 = "65P7iz6X5yEr1cwcgvQxbbIw7Uk3gOy5dIdtZ4rDveLqhrdJP+Li/Hx6tyK0NEb+2GCyneCMJiGqrADCSNk8sQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_tough_cookie___tough_cookie_4.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_tough_cookie___tough_cookie_4.0.0.tgz";
        url  = "https://registry.npmjs.org/tough-cookie/-/tough-cookie-4.0.0.tgz";
        sha512 = "tHdtEpQCMrc1YLrMaqXXcj6AxhYi/xgit6mZu1+EDWUn+qhUf8wMQoFIy9NXuq23zAwtcB0t/MjACGR18pcRbg==";
      };
    }
    {
      name = "https___registry.npmjs.org_tr46___tr46_2.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_tr46___tr46_2.1.0.tgz";
        url  = "https://registry.npmjs.org/tr46/-/tr46-2.1.0.tgz";
        sha512 = "15Ih7phfcdP5YxqiB+iDtLoaTz4Nd35+IiAv0kQ5FNKHzXgdWqPoTIqEDDJmXceQt4JZk6lVPT8lnDlPpGDppw==";
      };
    }
    {
      name = "https___registry.npmjs.org_ts_jest___ts_jest_27.1.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ts_jest___ts_jest_27.1.3.tgz";
        url  = "https://registry.npmjs.org/ts-jest/-/ts-jest-27.1.3.tgz";
        sha512 = "6Nlura7s6uM9BVUAoqLH7JHyMXjz8gluryjpPXxr3IxZdAXnU6FhjvVLHFtfd1vsE1p8zD1OJfskkc0jhTSnkA==";
      };
    }
    {
      name = "https___registry.npmjs.org_ts_node___ts_node_10.7.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ts_node___ts_node_10.7.0.tgz";
        url  = "https://registry.npmjs.org/ts-node/-/ts-node-10.7.0.tgz";
        sha512 = "TbIGS4xgJoX2i3do417KSaep1uRAW/Lu+WAL2doDHC0D6ummjirVOXU5/7aiZotbQ5p1Zp9tP7U6cYhA0O7M8A==";
      };
    }
    {
      name = "https___registry.npmjs.org_type_check___type_check_0.3.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_type_check___type_check_0.3.2.tgz";
        url  = "https://registry.npmjs.org/type-check/-/type-check-0.3.2.tgz";
        sha1 = "WITKtRLPHTVeP7eE8wgEsrUg23I=";
      };
    }
    {
      name = "https___registry.npmjs.org_type_detect___type_detect_4.0.8.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_type_detect___type_detect_4.0.8.tgz";
        url  = "https://registry.npmjs.org/type-detect/-/type-detect-4.0.8.tgz";
        sha512 = "0fr/mIH1dlO+x7TlcMy+bIDqKPsw/70tVyeHW787goQjhmqaZe10uwLujubK9q9Lg6Fiho1KUKDYz0Z7k7g5/g==";
      };
    }
    {
      name = "https___registry.npmjs.org_type_fest___type_fest_0.21.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_type_fest___type_fest_0.21.3.tgz";
        url  = "https://registry.npmjs.org/type-fest/-/type-fest-0.21.3.tgz";
        sha512 = "t0rzBq87m3fVcduHDUFhKmyyX+9eo6WQjZvf51Ea/M0Q7+T374Jp1aUiyUl0GKxp8M/OETVHSDvmkyPgvX+X2w==";
      };
    }
    {
      name = "https___registry.npmjs.org_typedarray_to_buffer___typedarray_to_buffer_3.1.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_typedarray_to_buffer___typedarray_to_buffer_3.1.5.tgz";
        url  = "https://registry.npmjs.org/typedarray-to-buffer/-/typedarray-to-buffer-3.1.5.tgz";
        sha512 = "zdu8XMNEDepKKR+XYOXAVPtWui0ly0NtohUscw+UmaHiAWT8hrV1rr//H6V+0DvJ3OQ19S979M0laLfX8rm82Q==";
      };
    }
    {
      name = "https___registry.npmjs.org_typescript___typescript_4.6.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_typescript___typescript_4.6.2.tgz";
        url  = "https://registry.npmjs.org/typescript/-/typescript-4.6.2.tgz";
        sha512 = "HM/hFigTBHZhLXshn9sN37H085+hQGeJHJ/X7LpBWLID/fbc2acUMfU+lGD98X81sKP+pFa9f0DZmCwB9GnbAg==";
      };
    }
    {
      name = "https___registry.npmjs.org_universalify___universalify_0.1.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_universalify___universalify_0.1.2.tgz";
        url  = "https://registry.npmjs.org/universalify/-/universalify-0.1.2.tgz";
        sha512 = "rBJeI5CXAlmy1pV+617WB9J63U6XcazHHF2f2dbJix4XzpUF0RS3Zbj0FGIOCAva5P/d/GBOYaACQ1w+0azUkg==";
      };
    }
    {
      name = "https___registry.npmjs.org_util_deprecate___util_deprecate_1.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_util_deprecate___util_deprecate_1.0.2.tgz";
        url  = "https://registry.npmjs.org/util-deprecate/-/util-deprecate-1.0.2.tgz";
        sha1 = "RQ1Nyfpw3nMnYvvS1KKJgUGaDM8=";
      };
    }
    {
      name = "https___registry.npmjs.org_v8_compile_cache_lib___v8_compile_cache_lib_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_v8_compile_cache_lib___v8_compile_cache_lib_3.0.0.tgz";
        url  = "https://registry.npmjs.org/v8-compile-cache-lib/-/v8-compile-cache-lib-3.0.0.tgz";
        sha512 = "mpSYqfsFvASnSn5qMiwrr4VKfumbPyONLCOPmsR3A6pTY/r0+tSaVbgPWSAIuzbk3lCTa+FForeTiO+wBQGkjA==";
      };
    }
    {
      name = "https___registry.npmjs.org_v8_to_istanbul___v8_to_istanbul_8.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_v8_to_istanbul___v8_to_istanbul_8.1.1.tgz";
        url  = "https://registry.npmjs.org/v8-to-istanbul/-/v8-to-istanbul-8.1.1.tgz";
        sha512 = "FGtKtv3xIpR6BYhvgH8MI/y78oT7d8Au3ww4QIxymrCtZEh5b8gCw2siywE+puhEmuWKDtmfrvF5UlB298ut3w==";
      };
    }
    {
      name = "https___registry.npmjs.org_w3c_hr_time___w3c_hr_time_1.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_w3c_hr_time___w3c_hr_time_1.0.2.tgz";
        url  = "https://registry.npmjs.org/w3c-hr-time/-/w3c-hr-time-1.0.2.tgz";
        sha512 = "z8P5DvDNjKDoFIHK7q8r8lackT6l+jo/Ye3HOle7l9nICP9lf1Ci25fy9vHd0JOWewkIFzXIEig3TdKT7JQ5fQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_w3c_keyname___w3c_keyname_2.2.4.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_w3c_keyname___w3c_keyname_2.2.4.tgz";
        url  = "https://registry.npmjs.org/w3c-keyname/-/w3c-keyname-2.2.4.tgz";
        sha512 = "tOhfEwEzFLJzf6d1ZPkYfGj+FWhIpBux9ppoP3rlclw3Z0BZv3N7b7030Z1kYth+6rDuAsXUFr+d0VE6Ed1ikw==";
      };
    }
    {
      name = "https___registry.npmjs.org_w3c_xmlserializer___w3c_xmlserializer_2.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_w3c_xmlserializer___w3c_xmlserializer_2.0.0.tgz";
        url  = "https://registry.npmjs.org/w3c-xmlserializer/-/w3c-xmlserializer-2.0.0.tgz";
        sha512 = "4tzD0mF8iSiMiNs30BiLO3EpfGLZUT2MSX/G+o7ZywDzliWQ3OPtTZ0PTC3B3ca1UAf4cJMHB+2Bf56EriJuRA==";
      };
    }
    {
      name = "https___registry.npmjs.org_walker___walker_1.0.8.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_walker___walker_1.0.8.tgz";
        url  = "https://registry.npmjs.org/walker/-/walker-1.0.8.tgz";
        sha512 = "ts/8E8l5b7kY0vlWLewOkDXMmPdLcVV4GmOQLyxuSswIJsweeFZtAsMF7k1Nszz+TYBQrlYRmzOnr398y1JemQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_webidl_conversions___webidl_conversions_5.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_webidl_conversions___webidl_conversions_5.0.0.tgz";
        url  = "https://registry.npmjs.org/webidl-conversions/-/webidl-conversions-5.0.0.tgz";
        sha512 = "VlZwKPCkYKxQgeSbH5EyngOmRp7Ww7I9rQLERETtf5ofd9pGeswWiOtogpEO850jziPRarreGxn5QIiTqpb2wA==";
      };
    }
    {
      name = "https___registry.npmjs.org_webidl_conversions___webidl_conversions_6.1.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_webidl_conversions___webidl_conversions_6.1.0.tgz";
        url  = "https://registry.npmjs.org/webidl-conversions/-/webidl-conversions-6.1.0.tgz";
        sha512 = "qBIvFLGiBpLjfwmYAaHPXsn+ho5xZnGvyGvsarywGNc8VyQJUMHJ8OBKGGrPER0okBeMDaan4mNBlgBROxuI8w==";
      };
    }
    {
      name = "https___registry.npmjs.org_whatwg_encoding___whatwg_encoding_1.0.5.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_whatwg_encoding___whatwg_encoding_1.0.5.tgz";
        url  = "https://registry.npmjs.org/whatwg-encoding/-/whatwg-encoding-1.0.5.tgz";
        sha512 = "b5lim54JOPN9HtzvK9HFXvBma/rnfFeqsic0hSpjtDbVxR3dJKLc+KB4V6GgiGOvl7CY/KNh8rxSo9DKQrnUEw==";
      };
    }
    {
      name = "https___registry.npmjs.org_whatwg_mimetype___whatwg_mimetype_2.3.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_whatwg_mimetype___whatwg_mimetype_2.3.0.tgz";
        url  = "https://registry.npmjs.org/whatwg-mimetype/-/whatwg-mimetype-2.3.0.tgz";
        sha512 = "M4yMwr6mAnQz76TbJm914+gPpB/nCwvZbJU28cUD6dR004SAxDLOOSUaB1JDRqLtaOV/vi0IC5lEAGFgrjGv/g==";
      };
    }
    {
      name = "https___registry.npmjs.org_whatwg_url___whatwg_url_8.7.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_whatwg_url___whatwg_url_8.7.0.tgz";
        url  = "https://registry.npmjs.org/whatwg-url/-/whatwg-url-8.7.0.tgz";
        sha512 = "gAojqb/m9Q8a5IV96E3fHJM70AzCkgt4uXYX2O7EmuyOnLrViCQlsEBmF9UQIu3/aeAIp2U17rtbpZWNntQqdg==";
      };
    }
    {
      name = "https___registry.npmjs.org_which___which_1.3.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_which___which_1.3.1.tgz";
        url  = "https://registry.npmjs.org/which/-/which-1.3.1.tgz";
        sha512 = "HxJdYWq1MTIQbJ3nw0cqssHoTNU267KlrDuGZ1WYlxDStUtKUhOaJmh112/TZmHxxUfuJqPXSOm7tDyas0OSIQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_which___which_2.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_which___which_2.0.2.tgz";
        url  = "https://registry.npmjs.org/which/-/which-2.0.2.tgz";
        sha512 = "BLI3Tl1TW3Pvl70l3yq3Y64i+awpwXqsGBYWkkqMtnbXgrMD+yj7rhW0kuEDxzJaYXGjEW5ogapKNMEKNMjibA==";
      };
    }
    {
      name = "https___registry.npmjs.org_word_wrap___word_wrap_1.2.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_word_wrap___word_wrap_1.2.3.tgz";
        url  = "https://registry.npmjs.org/word-wrap/-/word-wrap-1.2.3.tgz";
        sha512 = "Hz/mrNwitNRh/HUAtM/VT/5VH+ygD6DV7mYKZAtHOrbs8U7lvPS6xf7EJKMF0uW1KJCl0H701g3ZGus+muE5vQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_wrap_ansi___wrap_ansi_7.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_wrap_ansi___wrap_ansi_7.0.0.tgz";
        url  = "https://registry.npmjs.org/wrap-ansi/-/wrap-ansi-7.0.0.tgz";
        sha512 = "YVGIj2kamLSTxw6NsZjoBxfSwsn0ycdesmc4p+Q21c5zPuZ1pl+NfxVdxPtdHvmNVOQ6XSYG4AUtyt/Fi7D16Q==";
      };
    }
    {
      name = "https___registry.npmjs.org_wrappy___wrappy_1.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_wrappy___wrappy_1.0.2.tgz";
        url  = "https://registry.npmjs.org/wrappy/-/wrappy-1.0.2.tgz";
        sha1 = "tSQ9jz7BqjXxNkYFvA0QNuMKtp8=";
      };
    }
    {
      name = "https___registry.npmjs.org_write_file_atomic___write_file_atomic_3.0.3.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_write_file_atomic___write_file_atomic_3.0.3.tgz";
        url  = "https://registry.npmjs.org/write-file-atomic/-/write-file-atomic-3.0.3.tgz";
        sha512 = "AvHcyZ5JnSfq3ioSyjrBkH9yW4m7Ayk8/9My/DD9onKeu/94fwrMocemO2QAJFAlnnDN+ZDS+ZjAR5ua1/PV/Q==";
      };
    }
    {
      name = "https___registry.npmjs.org_ws___ws_7.5.7.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_ws___ws_7.5.7.tgz";
        url  = "https://registry.npmjs.org/ws/-/ws-7.5.7.tgz";
        sha512 = "KMvVuFzpKBuiIXW3E4u3mySRO2/mCHSyZDJQM5NQ9Q9KHWHWh0NHgfbRMLLrceUK5qAL4ytALJbpRMjixFZh8A==";
      };
    }
    {
      name = "https___registry.npmjs.org_xml_name_validator___xml_name_validator_3.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_xml_name_validator___xml_name_validator_3.0.0.tgz";
        url  = "https://registry.npmjs.org/xml-name-validator/-/xml-name-validator-3.0.0.tgz";
        sha512 = "A5CUptxDsvxKJEU3yO6DuWBSJz/qizqzJKOMIfUJHETbBw/sFaDxgd6fxm1ewUaM0jZ444Fc5vC5ROYurg/4Pw==";
      };
    }
    {
      name = "https___registry.npmjs.org_xmlchars___xmlchars_2.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_xmlchars___xmlchars_2.2.0.tgz";
        url  = "https://registry.npmjs.org/xmlchars/-/xmlchars-2.2.0.tgz";
        sha512 = "JZnDKK8B0RCDw84FNdDAIpZK+JuJw+s7Lz8nksI7SIuU3UXJJslUthsi+uWBUYOwPFwW7W7PRLRfUKpxjtjFCw==";
      };
    }
    {
      name = "https___registry.npmjs.org_xtend___xtend_4.0.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_xtend___xtend_4.0.2.tgz";
        url  = "https://registry.npmjs.org/xtend/-/xtend-4.0.2.tgz";
        sha512 = "LKYU1iAXJXUgAXn9URjiu+MWhyUXHsvfp7mcuYm9dSUKK0/CjtrUwFAxD82/mCWbtLsGjFIad0wIsod4zrTAEQ==";
      };
    }
    {
      name = "https___registry.npmjs.org_y18n___y18n_5.0.8.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_y18n___y18n_5.0.8.tgz";
        url  = "https://registry.npmjs.org/y18n/-/y18n-5.0.8.tgz";
        sha512 = "0pfFzegeDWJHJIAmTLRP2DwHjdF5s7jo9tuztdQxAhINCdvS+3nGINqPd00AphqJR/0LhANUS6/+7SCb98YOfA==";
      };
    }
    {
      name = "https___registry.npmjs.org_yallist___yallist_4.0.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_yallist___yallist_4.0.0.tgz";
        url  = "https://registry.npmjs.org/yallist/-/yallist-4.0.0.tgz";
        sha512 = "3wdGidZyq5PB084XLES5TpOSRA3wjXAlIWMhum2kRcv/41Sn2emQ0dycQW4uZXLejwKvg6EsvbdlVL+FYEct7A==";
      };
    }
    {
      name = "https___registry.npmjs.org_yaml___yaml_1.10.2.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_yaml___yaml_1.10.2.tgz";
        url  = "https://registry.npmjs.org/yaml/-/yaml-1.10.2.tgz";
        sha512 = "r3vXyErRCYJ7wg28yvBY5VSoAF8ZvlcW9/BwUzEtUsjvX/DKs24dIkuwjtuprwJJHsbyUbLApepYTR1BN4uHrg==";
      };
    }
    {
      name = "https___registry.npmjs.org_yargs_parser___yargs_parser_20.2.9.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_yargs_parser___yargs_parser_20.2.9.tgz";
        url  = "https://registry.npmjs.org/yargs-parser/-/yargs-parser-20.2.9.tgz";
        sha512 = "y11nGElTIV+CT3Zv9t7VKl+Q3hTQoT9a1Qzezhhl6Rp21gJ/IVTW7Z3y9EWXhuUBC2Shnf+DX0antecpAwSP8w==";
      };
    }
    {
      name = "https___registry.npmjs.org_yargs___yargs_16.2.0.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_yargs___yargs_16.2.0.tgz";
        url  = "https://registry.npmjs.org/yargs/-/yargs-16.2.0.tgz";
        sha512 = "D1mvvtDG0L5ft/jGWkLpG1+m0eQxOfaBvTNELraWj22wSVUMWxZUvYgJYcKh6jGGIkJFhH4IZPQhR4TKpc8mBw==";
      };
    }
    {
      name = "https___registry.npmjs.org_yn___yn_3.1.1.tgz";
      path = fetchurl {
        name = "https___registry.npmjs.org_yn___yn_3.1.1.tgz";
        url  = "https://registry.npmjs.org/yn/-/yn-3.1.1.tgz";
        sha512 = "Ux4ygGWsu2c7isFWe8Yu1YluJmqVhxqK2cLXNQA5AcC3QfbGNpM7fu0Y8b/z16pXLnFxZYvWhd3fhBY9DLmC6Q==";
      };
    }
  ];
}
